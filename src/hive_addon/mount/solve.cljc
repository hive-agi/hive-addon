(ns hive-addon.mount.solve
  "Pure DAG solver for the addon mounter — spec set -> MountPlan. No IO, no var
   resolution.

   Ordering constraints are an OCP rule-chain: IDependencyRule turns the spec set
   into directed [from-id to-id] edges (from mounts before to); `edges` folds the
   chain into one edge set. A new constraint is a new rule conj'd onto the chain
   — `solve` never changes to add one. Two built-in rules cover hard id deps
   (:addon/dependencies) and looser capability deps (:addon/requires-capabilities
   satisfied by any peer whose :addon/capabilities contains the cap).

   `solve` is a pure deterministic fn of the spec SET: it Kahn topo-sorts with a
   lowest-:addon/id lexicographic tie-break, so shuffled input yields an
   identical :ordered. Cycles/missing-deps/unmet-capabilities are diagnosed as
   data. Graceful by default (acyclic subset ordered, cycles reported); opt-in
   :fail-closed-cycles true returns (r/err :mount/unsolvable ...)."
  (:require [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; OCP dependency-rule chain — spec set -> directed edges #{[from-id to-id]}
;; =============================================================================

(defprotocol IDependencyRule
  "One ordering-constraint rule. Turns the spec set into a set of directed edges
   [from-id to-id], each meaning from-id must mount before to-id. Adding a
   constraint means adding a rule — no existing rule, and not `edges`/`solve`,
   has to change."
  (-edges [this specs]
    "Spec set -> #{[from-id to-id] ...} directed edges this rule contributes."))

(defrecord IdDependencyRule []
  IDependencyRule
  (-edges [_ specs]
    (let [ids (into #{} (map :addon/id) specs)]
      (into #{}
            (for [s specs
                  d (:addon/dependencies s #{})
                  :when (contains? ids d)]
              [d (:addon/id s)])))))

(defrecord CapabilityDependencyRule []
  IDependencyRule
  (-edges [_ specs]
    (into #{}
          (for [s     specs
                c     (:addon/requires-capabilities s #{})
                p     specs
                :when (and (not= (:addon/id p) (:addon/id s))
                           (contains? (:addon/capabilities p #{}) c))]
            [(:addon/id p) (:addon/id s)]))))

(def default-rules
  "The built-in ordering-rule chain. Order is not significant — `edges` unions
   every rule's contribution into one edge set."
  [(->IdDependencyRule)
   (->CapabilityDependencyRule)])

(defn edges
  "Fold a rule chain over the spec set into one directed edge set
   #{[from-id to-id] ...}. Defaults to default-rules."
  ([specs] (edges specs default-rules))
  ([specs rules]
   (into #{} (mapcat #(-edges % specs)) rules)))

;; =============================================================================
;; Pure Kahn topo-sort with stable lowest-id tie-break
;; =============================================================================

(defn- topo-sort
  "Kahn topo-sort over `ids` given directed `edge-set` #{[from to]}. Among
   zero-in-degree candidates always dequeues the LOWEST id lexicographically, so
   the result is a pure deterministic fn of the inputs (not insertion order).
   Returns [ordered-ids cyclic-ids]; cyclic-ids are the ids never dequeued
   (participate in / are downstream of a cycle)."
  [ids edge-set]
  (let [edge-set (into #{} (filter (fn [[from to]]
                                     (and (contains? ids from) (contains? ids to))))
                       edge-set)
        indeg0   (reduce (fn [m [_from to]] (update m to inc))
                         (zipmap ids (repeat 0))
                         edge-set)
        adj      (reduce (fn [m [from to]] (update m from (fnil conj #{}) to))
                         {}
                         edge-set)]
    (loop [indeg   indeg0
           ordered []]
      (let [ready (sort (for [[id d] indeg :when (zero? d)] id))]
        (if-let [id (first ready)]
          (recur (reduce (fn [m to] (update m to dec))
                         (dissoc indeg id)
                         (adj id))
                 (conj ordered id))
          [ordered (set (keys indeg))])))))

;; =============================================================================
;; solve — spec set -> MountPlan (or r/err under :fail-closed-cycles)
;; =============================================================================

(defn solve
  "Pure deterministic solve of a spec SET into a MountPlan.

   opts: {:rules [rule ...]         (default default-rules)
          :fail-closed-cycles bool  (default false)}

   Computes ordering edges via the folded rule chain, Kahn topo-sorts with a
   lowest-:addon/id tie-break, and diagnoses:
     :cycles              ids that could not be ordered (in/downstream of a cycle)
     :missing             id -> declared dep ids absent from the spec set
     :unmet-capabilities  id -> required caps no active spec provides
   Missing deps and unmet caps are reported but do NOT drop the spec from
   :ordered (graceful); cyclic ids are excluded from :ordered.

   Graceful default returns a MountPlan even with cycles. :fail-closed-cycles
   true returns (r/err :mount/unsolvable {:cycles ...}) when cycles exist."
  ([specs] (solve specs {}))
  ([specs {:keys [rules fail-closed-cycles]
           :or   {rules default-rules fail-closed-cycles false}}]
   (let [specs    (set specs)
         by-id    (into {} (map (juxt :addon/id identity)) specs)
         ids      (set (keys by-id))
         edge-set (edges specs rules)
         [ordered-ids cyclic] (topo-sort ids edge-set)
         ordered  (into [] (map by-id) ordered-ids)
         missing  (into {}
                        (keep (fn [s]
                                (let [absent (into #{} (remove ids) (:addon/dependencies s #{}))]
                                  (when (seq absent) [(:addon/id s) absent]))))
                        specs)
         provided (into #{} (mapcat :addon/capabilities) specs)
         unmet    (into {}
                        (keep (fn [s]
                                (let [need (into #{} (remove provided)
                                                 (:addon/requires-capabilities s #{}))]
                                  (when (seq need) [(:addon/id s) need]))))
                        specs)]
     (if (and fail-closed-cycles (seq cyclic))
       (r/err :mount/unsolvable {:cycles cyclic})
       {:ordered            ordered
        :cycles             cyclic
        :missing            missing
        :unmet-capabilities unmet}))))
