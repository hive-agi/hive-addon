(ns hive-addon.mount.solve
  "Pure DAG solver for the addon mounter — spec set -> MountPlan. No IO, no var
   resolution.

   Ordering constraints are a rule-chain: IDependencyRule turns the spec set
   into directed [from-id to-id] edges (from mounts before to); `edges` folds the
   chain into one edge set. Two built-in rules cover hard id deps
   (:addon/dependencies) and looser capability deps (:addon/requires-capabilities
   satisfied by any peer whose :addon/capabilities contains the cap).

   `solve` is a pure deterministic fn of the spec SET: it Kahn topo-sorts with a
   lowest-:addon/id lexicographic tie-break, so shuffled input yields an
   identical :ordered. Cycles/missing-deps/unmet-capabilities are diagnosed as
   data. Graceful by default (acyclic subset ordered, cycles reported); opt-in
   :fail-closed-cycles true returns (r/err :mount/unsolvable ...).

   PORTABLE STRATUM. This namespace is in the require closure of the hot-reload
   core and must load and BEHAVE IDENTICALLY on the JVM, cljw and cljrs. Three
   admission rules follow, each mechanically checkable and each derived from a
   measured divergence rather than from caution:

   - Zero reader conditionals and zero host interop. `:clj` does not select the
     JVM: cljw presents `:clj` and cljrs presents `:rust`.
   - No `for`. On cljrs a second binding and `:let` are unbound-symbol errors,
     and `:when` is SILENTLY IGNORED — in `topo-sort` that would emit a wrong
     mount order while reporting success. `mapcat`/`keep`/`reduce` say the same
     thing and are cleared on all three.
   - No `:or` destructuring default that is not SELF-EVALUATING. cljrs does not
     evaluate the default, so a symbol default binds the symbol itself; `rules`
     became the symbol `default-rules`, `edges` folded over a symbol into an
     EMPTY edge set, and solve returned a lexicographic order reporting no
     cycles. Literal defaults (`false`, `0`, `:kw`) are unaffected, which is
     exactly what hides the bug. Use `(or x default)` in the body.

   The three rules are enforced by hive-addon.mount.portable-test, which reads
   this stratum's source and fails on a violation, and the behaviour is pinned
   by the tri-runtime differential oracle in test/portable/oracle.cljc."
  (:require [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; OCP dependency-rule chain — spec set -> directed edges #{[from-id to-id]}
;; =============================================================================

(defprotocol IDependencyRule
  "One ordering-constraint rule. -edges turns the spec set into a set of directed
   edges [from-id to-id], each meaning from-id must mount before to-id."
  (-edges [this specs]
    "Spec set -> #{[from-id to-id] ...} directed edges this rule contributes."))

(defrecord IdDependencyRule []
  IDependencyRule
  (-edges [_ specs]
    ;; `mapcat`/`keep` rather than a multi-binding `for`: the portable stratum
    ;; may only name constructs the tri-runtime idiom probe has cleared, and
    ;; `for` is not one of them (see the ns docstring).
    (let [ids (into #{} (map :addon/id) specs)]
      (into #{}
            (mapcat (fn [s]
                      (let [to (:addon/id s)]
                        (keep (fn [d] (when (contains? ids d) [d to]))
                              (:addon/dependencies s #{})))))
            specs))))

(defrecord CapabilityDependencyRule []
  IDependencyRule
  (-edges [_ specs]
    ;; One edge per (provider, consumer) PAIR rather than per (consumer,
    ;; capability, provider) triple. The triple form collapsed to the same set
    ;; anyway — a provider satisfying two of a consumer's capabilities
    ;; contributes one edge either way — so `some` over the capabilities is the
    ;; same relation with one less level of iteration.
    (into #{}
          (mapcat (fn [s]
                    (let [sid  (:addon/id s)
                          caps (:addon/requires-capabilities s #{})]
                      (when (seq caps)
                        (keep (fn [p]
                                (let [pid (:addon/id p)]
                                  (when (and (not= pid sid)
                                             (some (fn [c]
                                                     (contains? (:addon/capabilities p #{}) c))
                                                   caps))
                                    [pid sid])))
                              specs)))))
          specs)))

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
      ;; `keep` rather than `for ... :when`: a runtime that drops the :when
      ;; clause would dequeue a node with non-zero in-degree and emit a WRONG
      ;; order while reporting success (see the ns docstring).
      (let [ready (sort (keep (fn [[id d]] (when (zero? d) id)) indeg))]
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
     :duplicates          id -> count, for any :addon/id shared by >1 spec
   Missing deps and unmet caps are reported but do NOT drop the spec from
   :ordered (graceful); a duplicated id keeps one surviving spec in :ordered;
   cyclic ids are excluded from :ordered.

   Graceful default returns a MountPlan even with cycles. :fail-closed-cycles
   true returns (r/err :mount/unsolvable {:cycles ...}) when cycles exist."
  ([specs] (solve specs {}))
  ([specs {:keys [rules fail-closed-cycles]}]
   ;; `(or rules default-rules)` rather than an `:or` default: on cljrs an :or
   ;; default is not EVALUATED, so a symbol default binds the symbol itself.
   ;; `rules` would become the symbol `default-rules`, `edges` would fold over a
   ;; symbol and yield an EMPTY edge set, and solve would silently return a
   ;; lexicographic order with no cycles detected. Self-evaluating defaults
   ;; (`fail-closed-cycles false`) are unaffected, which is what makes the bug
   ;; invisible until a symbol default is used.
   (let [rules      (or rules default-rules)
         specs      (set specs)
         by-id      (into {} (map (juxt :addon/id identity)) specs)
         ids        (set (keys by-id))
         duplicates (into {}
                          (keep (fn [[id ss]] (when (> (count ss) 1) [id (count ss)])))
                          (group-by :addon/id specs))
         edge-set   (edges specs rules)
         [ordered-ids cyclic] (topo-sort ids edge-set)
         ordered    (into [] (map by-id) ordered-ids)
         missing    (into {}
                          (keep (fn [s]
                                  (let [absent (into #{} (remove ids) (:addon/dependencies s #{}))]
                                    (when (seq absent) [(:addon/id s) absent]))))
                          specs)
         provided   (into #{} (mapcat :addon/capabilities) specs)
         unmet      (into {}
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
        :unmet-capabilities unmet
        :duplicates         duplicates}))))
