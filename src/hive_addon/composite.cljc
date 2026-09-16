(ns hive-addon.composite
  "A COMPOSITE IAddon — an addon whose members are themselves addons.

   Every host registry in the ecosystem is FLAT: a host holds addons, and an
   addon holds tools. There was no way to say \"this one addon IS a catalog of
   addons\", so a family of related capabilities had to arrive as N sibling
   entries in the host registry, versioned, mounted and reasoned about apart.

   A CompositeAddon closes that. It satisfies IAddon on its outer face, so a
   host mounts it as ONE addon; inside, it owns a private IMountHost
   sub-registry and drives its members through the same protocol. Because the
   composite is itself an IAddon, a composite may be a MEMBER of another
   composite, to arbitrary depth — the structure is a tree, not two layers.

   What is projected upward, and why the two halves differ:

   - DECLARATIONS (capabilities) are the union over EVERY member, initialized
     or not. A capability is a static claim about what the addon offers, and a
     host reads it for routing before anything is initialized.
   - CONTRIBUTIONS (tools, schema-extensions, hooks, excluded-tools) come from
     INITIALIZED members only. A member that failed to start must not advertise
     a tool it cannot serve; that is the difference between a degraded catalog
     and a lying one.

   Member failure is GRACEFUL by default, the same bargain hive-addon.mount
   already strikes: a member that fails to initialize is recorded, the rest
   still mount, and the composite reports :degraded rather than vanishing. Set
   :on-member-failure :fail to make any member failure fail the composite.

   Ordering is hive-addon.mount.solve, reused rather than reimplemented: a
   member may declare :member/depends-on over its siblings and the solver
   topologically orders them, with the same graceful cycle handling.

   Pure .cljc — no JVM-only construction path. Members arrive as INSTANCES, so
   the composite never resolves a ctor, reads a manifest or touches a
   classpath. A host that wants classpath discovery already has
   hive-addon.mount.compose; this namespace is what that machinery mounts INTO.

   Rationale lives in hive memory (KG-linked), not here."
  (:require [clojure.string :as str]
            [hive-addon.protocol :as proto]
            [hive-addon.mount.port :as port]
            [hive-addon.mount.solve :as solve]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Guarded protocol calls
;; =============================================================================

(defn- optional
  "Call `(f addon)`, answering `fallback` when the addon simply does not
   implement that optional protocol method. Any OTHER throwable is caught too
   and reported through `on-error`, because one broken member must never blind
   the whole catalog: a composite that propagates a member's exception out of
   `tools` takes down every sibling's tools with it."
  [f addon fallback on-error]
  (try
    (f addon)
    (catch #?(:clj Throwable :cljs :default) t
      (when-not (proto/unimplemented-method? t)
        (on-error t))
      fallback)))

(defn- nil-reporter [_] nil)

;; =============================================================================
;; Members — normalization and ordering
;; =============================================================================

(defn normalize-member
  "Coerce one member into the canonical member map.

   Accepts a bare IAddon instance, or a map carrying `:member/addon` plus any
   of `:member/id` (defaults to the addon's own id), `:member/config` (merged
   OVER the composite's config when that member initializes),
   `:member/depends-on` (sibling ids, fed to the solver) and `:member/prefix`
   (tool-name prefix override). Returns nil for anything that is not an addon,
   so a malformed entry is dropped rather than throwing at construction."
  [m]
  (cond
    (proto/addon? m)
    {:member/addon m :member/id (proto/addon-id m)}

    (and (map? m) (proto/addon? (:member/addon m)))
    (let [addon (:member/addon m)]
      (-> m
          (assoc :member/addon addon)
          (update :member/id #(or % (proto/addon-id addon)))))

    :else nil))

(defn normalize-members
  "Normalize a seq of members, dropping the ones that carry no addon."
  [ms]
  (into [] (keep normalize-member) ms))

(defn- member->spec
  "Project a member onto the subset of MountSpec keys the solver reads, so
   ordering is hive-addon.mount.solve's job rather than a second topo-sort."
  [{:member/keys [id depends-on addon]}]
  {:addon/id           id
   :addon/dependencies (set (or depends-on #{}))
   :addon/capabilities (optional proto/capabilities addon #{} nil-reporter)})

(defn order-members
  "Order members by their declared `:member/depends-on`, via the mount solver.

   Returns `{:ordered [member] :cycles #{id} :duplicates {id count}}`. Members
   caught in a cycle are EXCLUDED from `:ordered` and reported, matching the
   solver's graceful contract; the composite surfaces them as member failures."
  [members]
  (let [by-id (into {} (map (juxt :member/id identity)) members)
        plan  (solve/solve (mapv member->spec members))]
    {:ordered    (into [] (keep (comp by-id :addon/id)) (:ordered plan))
     :cycles     (set (:cycles plan))
     :duplicates (:duplicates plan)}))

;; =============================================================================
;; Tool naming
;; =============================================================================

(defn tool-prefix
  "The default tool-name prefix for a member id: its LAST dotted or slashed
   segment, lowercased, with every other character folded to `_`.

   \"hive.prompt.chain-of-thought\" -> \"chain_of_thought\". The last segment
   rather than the whole id because the host already namespaces the composite's
   own tools by the composite's id, so repeating the shared stem would spell
   every tool twice."
  [id]
  (-> (str id)
      (str/split #"[./]")
      last
      str/lower-case
      (str/replace #"[^a-z0-9]+" "_")
      (str/replace #"^_+|_+$" "")))

(defn qualify-tool
  "Prefix one tool-def's `:name`. A blank prefix leaves the name untouched."
  [prefix tool]
  (if (str/blank? prefix)
    tool
    (update tool :name #(str prefix "_" %))))

(defn collisions
  "Tool names claimed by more than one member, as `{name [member-id ...]}`.
   Empty when the surface is unambiguous."
  [named]
  (into {}
        (keep (fn [[nm entries]]
                (let [ids (distinct (map :member/id entries))]
                  (when (> (count ids) 1) [nm (vec ids)]))))
        (group-by (comp :name :tool) named)))

(defn aggregate-tools
  "Fold member tool-defs into one surface.

   `:prefix` (the default) qualifies every tool with its member's prefix, so a
   catalog of siblings that all export `render` stays addressable. `:flat`
   leaves names alone, which is what a curated catalog with no overlap wants.

   Returns `{:tools [tool-def] :collisions {name [member-id ...]}}`. Collisions
   are REPORTED, never silently resolved: under `:flat` two members claiming
   one name is a real ambiguity, and dropping one of them quietly is how a
   technique goes missing without anybody noticing."
  [members naming on-error]
  (let [named (for [{:member/keys [id addon prefix] :as m} members
                    tool (optional proto/tools addon [] on-error)
                    :let [p (if (= :flat naming) "" (or prefix (tool-prefix id)))]]
                {:member/id id :member m :tool (qualify-tool p tool)})]
    {:tools      (mapv :tool named)
     :collisions (collisions named)}))

;; =============================================================================
;; Health
;; =============================================================================

(defn fold-health
  "Fold member health into the composite's.

   `:ok` when every member is ok, `:down` when a composite that HAS members has
   no working one left, `:degraded` in between. A composite with no members is
   `:ok` — an empty catalog is empty, not broken."
  [statuses]
  (let [n (count statuses)]
    (cond
      (zero? n)                     :ok
      (every? #(= :ok %) statuses)  :ok
      (every? #(= :down %) statuses) :down
      :else                         :degraded)))

(defn- member-health
  "One member's status keyword, folding an unreachable or throwing member to
   :down rather than letting it throw through the composite's health."
  [addon on-error]
  (let [h (optional proto/health addon {:status :down} on-error)]
    (or (:status h) :down)))

;; =============================================================================
;; CompositeAddon
;; =============================================================================

(defn- init-member!
  "Register and initialize one member into the inner host. Returns
   `{:member/id id :success? bool :errors [...]}`; never throws."
  [host {:member/keys [id addon config]} base-config]
  (try
    (port/register! host addon)
    (let [res (port/init! host id (merge base-config config))]
      (if (or (nil? res) (:success? res))
        {:member/id id :success? true}
        {:member/id id :success? false
         :errors (vec (or (:errors res) [(str id ": initialize! returned :success? false")]))}))
    (catch #?(:clj Throwable :cljs :default) t
      {:member/id id :success? false
       :errors [(str id ": " (or (ex-message t) t))]})))

(defrecord CompositeAddon [id opts state]
  proto/IAddon

  (addon-id [_] id)

  (addon-type [_] (:addon-type opts :native))

  (capabilities [_]
    ;; DECLARED, so the union spans every member whether or not it started.
    (into (set (:capabilities opts #{}))
          (mapcat #(optional proto/capabilities (:member/addon %) #{} nil-reporter))
          (:members opts)))

  (initialize! [_ config]
    (if (:started? @state)
      {:success? true :already-initialized? true}
      (let [{:keys [ordered cycles duplicates]} (order-members (:members opts))
            host    (or (:host @state) (port/atom-mount-host))
            results (mapv #(init-member! host % config) ordered)
            live    (into [] (comp (filter :success?) (map :member/id)) results)
            failed  (into {} (comp (remove :success?)
                                   (map (juxt :member/id :errors)))
                          results)
            cyc     (into {} (map (fn [c] [c ["excluded from the mount order: dependency cycle"]])) cycles)
            failed  (merge failed cyc)
            live?   (set live)
            started (into [] (filter #(live? (:member/id %))) ordered)
            fail?   (and (seq failed) (= :fail (:on-member-failure opts :degrade)))]
        (reset! state {:started? (not fail?)
                       :host     host
                       :mounted  (mapv :member/id ordered)
                       :live     started
                       :failed   failed
                       :duplicates duplicates})
        (cond-> {:success? (not (or fail? (and (seq (:members opts)) (empty? live))))
                 :metadata {:members  (mapv :member/id (:members opts))
                            :mounted  live
                            :failed   (vec (keys failed))
                            :duplicates duplicates}}
          (seq failed) (assoc :errors (vec (mapcat val failed)))))))

  (shutdown! [_]
    (when-let [host (:host @state)]
      (doseq [mid (reverse (:mounted @state))]
        (try (port/shutdown! host mid)
             (catch #?(:clj Throwable :cljs :default) _ nil))))
    (reset! state {:started? false})
    nil)

  (tools [_]
    ;; CONTRIBUTED, so only members that actually started are advertised.
    (:tools (aggregate-tools (:live @state) (:tool-naming opts :prefix) nil-reporter)))

  (schema-extensions [_]
    (into []
          (mapcat #(optional proto/schema-extensions (:member/addon %) [] nil-reporter))
          (:live @state)))

  (health [_]
    (let [live (:live @state)
          per  (into {}
                     (map (fn [{:member/keys [id addon]}]
                            [id (member-health addon nil-reporter)]))
                     live)
          failed (:failed @state)
          statuses (concat (vals per) (repeat (count failed) :down))
          {:keys [collisions]} (aggregate-tools live (:tool-naming opts :prefix) nil-reporter)]
      {:status (if (:started? @state) (fold-health statuses) :down)
       :details (cond-> {:members (count (:members opts))
                         :live    (count live)
                         :health  per}
                  (seq failed)     (assoc :failed failed)
                  (seq collisions) (assoc :tool-collisions collisions))}))

  (excluded-tools [_]
    (into #{}
          (mapcat #(optional proto/excluded-tools (:member/addon %) #{} nil-reporter))
          (:live @state)))

  (hooks [_]
    ;; Earlier members win: mount order is the tie-break, so a hook contest is
    ;; resolved the same way every time rather than by map-merge accident.
    (reduce (fn [acc {:member/keys [addon]}]
              (merge (optional proto/hooks addon {} nil-reporter) acc))
            {}
            (:live @state))))

(defn composite
  "Build a CompositeAddon: one IAddon whose members are addons.

   opts:
     :members           seq of IAddon instances or member maps (see
                        `normalize-member`). Malformed entries are dropped.
     :capabilities      capabilities the composite declares in its OWN right;
                        members' capabilities are unioned on top.
     :addon-type        default :native.
     :tool-naming       :prefix (default) or :flat — see `aggregate-tools`.
     :on-member-failure :degrade (default) or :fail.

   The returned addon is inert until `initialize!`; members are mounted then,
   into a private IMountHost the composite owns."
  [id {:keys [members] :as opts}]
  (->CompositeAddon id
                    (assoc opts :members (normalize-members members))
                    (atom {:started? false})))

(defn composite?
  "True when `x` is a CompositeAddon. A host never needs this to MOUNT one —
   it is an IAddon like any other — but a diagnostic that walks the tree does."
  [x]
  (instance? CompositeAddon x))

(defn member-ids
  "The member ids of a composite, in declaration order. `[]` for a leaf addon,
   so a caller can walk a mixed tree without type-testing first."
  [x]
  (if (composite? x)
    (mapv :member/id (:members (:opts x)))
    []))

(defn members
  "The member maps of a composite, in declaration order. `[]` for a leaf addon.

   The instances, not just the ids: a diagnostic that walks the tree, or a
   domain that asks each member whether it also satisfies some protocol of its
   own, needs the objects. `member-ids` is this projected onto names."
  [x]
  (if (composite? x) (vec (:members (:opts x))) []))

(defn live-members
  "The members that INITIALIZED successfully, in mount order. `[]` before
   `initialize!`, after `shutdown!`, or for a leaf addon.

   This is the set whose contributions the composite actually projects upward,
   so it is also the honest answer to \"what is this catalog serving right now\"."
  [x]
  (if (composite? x) (vec (:live @(:state x))) []))
