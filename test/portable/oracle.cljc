(ns portable.oracle
  "Differential oracle for the hive-addon PORTABLE STRATUM.

   Emits one canonical line per observation. The JVM, cljw and cljrs must agree
   LINE FOR LINE; `test/portable/run.sh` runs all three and diffs them. Where
   hive-addon.mount.portable-test checks the stratum's SOURCE against three
   admission rules, this checks its BEHAVIOUR — the rules exist to keep this
   diff empty, and a rule with no behavioural consequence would be superstition.

   Everything emitted here is canonicalized: sets are sorted, records are
   reduced to their strategy id, and no whole map containing namespaced keys is
   printed raw. Set iteration order, record print-names and the `#:addon{...}`
   namespaced-map shorthand are host-dependent and are NOT behaviour — leaving
   them raw produces diffs that say nothing, which trains the reader to ignore
   the diff.

   The IMountDriver leg lives in oracle_driver.cljc because cljrs cannot yet
   implement a protocol from another namespace; see that file."
  (:require [hive-addon.hot.cascade :as cascade]
            [hive-addon.hot.strategy :as strat]
            [hive-addon.mount.solve :as solve]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn emit [label v] (println (str label " | " (pr-str v))))

(defn sorted-vals
  "Canonicalize {k #{...}} to {k [sorted...]} — set order is host-dependent."
  [m]
  (into {} (map (fn [[k v]] [k (vec (sort v))])) m))

(def specs
  "Deliberately NOT in dependency order, so :ordered is evidence about the solve
   rather than about the input's order."
  [{:addon/id "c" :addon/dependencies #{"b"}}
   {:addon/id "a"}
   {:addon/id "b" :addon/dependencies #{"a"}}
   {:addon/id "solo"}
   {:addon/id "cap-consumer" :addon/requires-capabilities #{:store}}
   {:addon/id "cap-provider" :addon/capabilities #{:store :extra}}])

(def cyclic
  [{:addon/id "x" :addon/dependencies #{"y"}}
   {:addon/id "y" :addon/dependencies #{"x"}}
   {:addon/id "free"}])

;; =============================================================================
;; solve
;; =============================================================================

(emit "edges"         (vec (sort (solve/edges specs))))
(emit "ordered"       (mapv :addon/id (:ordered (solve/solve specs))))
(emit "ordered/shuffled-input-stable"
      (= (mapv :addon/id (:ordered (solve/solve specs)))
         (mapv :addon/id (:ordered (solve/solve (reverse specs))))))
(emit "cycles"        (vec (sort (:cycles (solve/solve cyclic)))))
(emit "cycle/ordered" (mapv :addon/id (:ordered (solve/solve cyclic))))
(emit "missing"       (sorted-vals (:missing (solve/solve [{:addon/id "m" :addon/dependencies #{"gone"}}]))))
(emit "unmet-caps"    (sorted-vals (:unmet-capabilities (solve/solve [{:addon/id "u" :addon/requires-capabilities #{:nope}}]))))
;; two DISTINCT specs sharing an id: `solve` sets the spec collection first, so
;; two identical maps would collapse and the observation would be vacuous.
(emit "duplicates"    (:duplicates (solve/solve [{:addon/id "d" :addon/init-ns "one"}
                                                 {:addon/id "d" :addon/init-ns "two"}])))

;; =============================================================================
;; cascade
;; =============================================================================

(emit "dependents/a"    (vec (sort (cascade/dependents specs #{"a"}))))
(emit "dependents/b"    (vec (sort (cascade/dependents specs #{"b"}))))
(emit "dependents/solo" (vec (sort (cascade/dependents specs #{"solo"}))))
(emit "dependents/cap"  (vec (sort (cascade/dependents specs #{"cap-provider"}))))
(emit "affected-plan"   (mapv :addon/id (:ordered (cascade/affected-plan specs #{"a"} {}))))
(emit "ns->addon-ids"   (sorted-vals (cascade/ns->addon-ids [{:addon/id "p" :addon/init-ns "shared.ns"}
                                                             {:addon/id "q" :addon/init-ns "shared.ns"}])))
(emit "seeds-for-ns"    (vec (sort (cascade/seeds-for-ns [{:addon/id "p" :addon/init-ns "shared.ns"}] "shared.ns"))))

;; =============================================================================
;; strategy selection
;; =============================================================================

(emit "chain"           (mapv strat/-strategy-id (strat/installed-strategies)))
(emit "select/default"  (strat/-strategy-id (:ok (strat/select {:addon/id "a"} {}))))
(emit "select/declared" (strat/-strategy-id (:ok (strat/select {:addon/id "a" :addon/reload-strategy :inert} {}))))
(emit "select/unknown"  (:error (strat/select {:addon/id "a" :addon/reload-strategy :nope} {})))
(emit "select/jar-source"
      (strat/-strategy-id (:ok (strat/select {:addon/id "a"}
                                             {:hot/source {:hot/reloadable? false
                                                           :hot/source-kind :jar}}))))

;; =============================================================================
;; the refusal path — no driver, so nothing may be torn down
;; =============================================================================

(let [rep (strat/reload! {:addon/id "a"} {:hot/specs specs :hot/seeds #{"a"}})]
  (emit "refusal/ok?"      (:ok? rep))
  (emit "refusal/affected" (:hot/affected rep))
  (emit "refusal/errors"   (:errors rep)))

(println "ORACLE-END")
