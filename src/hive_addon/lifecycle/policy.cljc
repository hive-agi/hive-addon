(ns hive-addon.lifecycle.policy
  "Pure lifecycle decisions over a spec set: which addons mount at boot, which
   stay dormant, what an activation must mount, and what a sweep may evict.
   No IO, no clock, no host. Time arrives as a number.

   Dependency edges come from hive-addon.mount.solve/edges, the same rule chain
   the mounter orders by, so a lifecycle decision can never disagree with a
   mount order.

   Portable stratum: no reader conditionals, no `for`, only self-evaluating
   `:or` defaults (see hive-addon.mount.solve)."
  (:require [hive-addon.mount.solve :as solve]
            [hive-addon.lifecycle.surface :as surface]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def default-policy :eager)

(def default-idle-ms
  "30 minutes."
  1800000)

;; =============================================================================
;; Resolution
;; =============================================================================

(def policies #{:eager :lazy :pinned})

(defn- valid-keys
  "DECL without a :policy or :idle-ms it cannot honour."
  [decl]
  (when (map? decl)
    (cond-> (select-keys decl [:policy :idle-ms])
      (not (contains? policies (:policy decl))) (dissoc :policy)
      (not (and (int? (:idle-ms decl)) (pos? (:idle-ms decl)))) (dissoc :idle-ms))))

(defn resolve-lifecycle
  "The Lifecycle for SPEC. Precedence, strongest first: the per-id OVERRIDE,
   the manifest's :addon/lifecycle, the host DEFAULTS decl, the built-ins.
   A key a layer cannot honour is ignored in that layer."
  [spec defaults override]
  (let [decl (merge (valid-keys defaults)
                    (valid-keys (:addon/lifecycle spec))
                    (valid-keys override))]
    {:policy  (or (:policy decl) default-policy)
     :idle-ms (or (:idle-ms decl) default-idle-ms)}))

(defn resolve-all
  "{addon-id Lifecycle} for SPECS. OPTS: {:defaults decl :overrides {id decl}}."
  [specs opts]
  (let [defaults  (:defaults opts)
        overrides (or (:overrides opts) {})]
    (into {}
          (map (fn [spec]
                 [(:addon/id spec)
                  (resolve-lifecycle spec defaults (get overrides (:addon/id spec)))]))
          specs)))

;; =============================================================================
;; Graph
;; =============================================================================

(defn- index-edges
  "{:deps {to #{from}} :dependents {from #{to}}} from solve's edge set."
  [specs]
  (reduce (fn [acc [from to]]
            (-> acc
                (update-in [:deps to] (fnil conj #{}) from)
                (update-in [:dependents from] (fnil conj #{}) to)))
          {:deps {} :dependents {}}
          (solve/edges specs)))

(defn- closure
  "IDS plus everything reachable from them through ADJ."
  [adj ids]
  (loop [frontier (set ids)
         seen     (set ids)]
    (let [next-ids (into #{} (comp (mapcat #(get adj % #{})) (remove seen)) frontier)]
      (if (empty? next-ids)
        seen
        (recur next-ids (into seen next-ids))))))

(defn dependency-closure
  "IDS plus every addon they transitively depend on, within SPECS."
  [specs ids]
  (closure (:deps (index-edges specs)) ids))

(defn dependent-closure
  "IDS plus every addon that transitively depends on them, within SPECS."
  [specs ids]
  (closure (:dependents (index-edges specs)) ids))

(defn- ordered-ids
  "IDS in the mount order solve gives the whole SPECS set."
  [specs ids]
  (let [wanted (set ids)]
    (into [] (comp (map :addon/id) (filter wanted)) (:ordered (solve/solve specs)))))

;; =============================================================================
;; Boot
;; =============================================================================

(defn boot-partition
  "Split SPECS into what mounts at boot and what stays dormant.

   LIFECYCLES is {id Lifecycle}; SURFACE-OF answers an id's Surface or nil.

   A :lazy addon is downgraded to eager when
     :no-surface          nothing can be advertised for it, so nothing can try it
     :required-by-eager   something mounted at boot depends on it

   Returns {:eager [id ...] in mount order, :dormant [id ...] sorted,
            :downgraded {id reason}}."
  [specs lifecycles surface-of]
  (let [ids        (mapv :addon/id specs)
        lazy?      #(= :lazy (:policy (get lifecycles %)))
        no-surface (into #{} (comp (filter lazy?) (remove surface-of)) ids)
        seed       (into no-surface (remove lazy?) ids)
        eager      (dependency-closure specs seed)
        required   (into #{} (comp (filter lazy?) (remove no-surface) (filter eager)) ids)]
    {:eager      (ordered-ids specs eager)
     :dormant    (vec (sort (remove eager ids)))
     :downgraded (merge (zipmap required (repeat :required-by-eager))
                        (zipmap no-surface (repeat :no-surface)))}))

;; =============================================================================
;; Activation
;; =============================================================================

(defn activation-ids
  "What mounting ID requires: ID and its transitive dependencies that are not
   ACTIVE? yet, in mount order."
  [specs id active?]
  (ordered-ids specs (remove active? (dependency-closure specs #{id}))))

;; =============================================================================
;; Sweep
;; =============================================================================

(def keep-rules
  "Why a sweep KEEPS an addon, as an ordered rule table: [KeepReason keep?]
   where (keep? now state) answers true to keep it for that reason. The first
   rule that fires names the reason; an addon no rule keeps may be evicted on
   its own merits.

   Data, so a new reason is a new ROW — the verdict function never changes.

   :no-surface keeps an addon that nothing could advertise while it is dormant:
   evicting it would leave no stub to route a call back to it, so its hooks and
   its whole contribution would simply vanish and it would never re-mount."
  [[:pinned     (fn [_ st] (= :pinned (get-in st [:lifecycle :policy])))]
   [:eager      (fn [_ st] (= :eager (get-in st [:lifecycle :policy])))]
   [:not-active (fn [_ st] (not= :active (:phase st)))]
   [:no-surface (fn [_ st] (= :none (:surface/source st)))]
   [:in-flight  (fn [_ st] (pos? (or (:in-flight st) 0)))]
   [:never-used (fn [_ st] (nil? (:last-used-ms st)))]
   [:fresh      (fn [now st] (< (- now (:last-used-ms st)) (get-in st [:lifecycle :idle-ms])))]])

(defn- idle-verdict
  "nil when STATE may be evicted at NOW on its own merits, else the KeepReason
   of the first rule in `keep-rules` that keeps it."
  [now state]
  (some (fn [[reason keep?]] (when (keep? now state) reason)) keep-rules))

(defn lazy-permitted?
  "May an addon with SURFACE (nil when it has none) be :lazy, i.e. dormant
   behind stubs? Only when something can be advertised for it: a hooks-only or
   surface-less addon has nothing a caller could reach to wake it."
  [surface]
  (not (surface/blank? surface)))

(defn sweep-plan
  "What a sweep at NOW may evict. STATES is {id UseState}.

   An idle addon is still kept while any ACTIVE addon depends on it. Evicting a
   dependent frees its dependencies in the same sweep, so the plan is a fixpoint
   and :evict is ordered dependents-first.

   Returns a SweepPlan."
  [now specs states]
  (let [adj        (:dependents (index-edges specs))
        dependents (into {} (map (fn [id] [id (disj (closure adj #{id}) id)])) (keys states))
        verdicts   (into {} (map (fn [[id st]] [id (idle-verdict now st)])) states)
        candidate? (into #{} (comp (filter (fn [[_ v]] (nil? v))) (map key)) verdicts)
        active0    (into #{} (comp (filter (fn [[_ st]] (= :active (:phase st)))) (map key)) states)]
    (loop [active active0
           evict  []]
      (let [free (->> active
                      (filter candidate?)
                      (remove (fn [id] (some #(contains? active %) (get dependents id #{}))))
                      sort
                      vec)]
        (if (empty? free)
          (let [evicted (set evict)]
            {:now-ms now
             :evict  evict
             :kept   (into {}
                           (comp (remove (fn [[id _]] (contains? evicted id)))
                                 (map (fn [[id v]] [id (or v :dependent-active)])))
                           verdicts)})
          (recur (reduce disj active free) (into evict free)))))))
