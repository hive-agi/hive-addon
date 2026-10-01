(ns hive-addon.hot-trifecta-test
  "Trifecta coverage for the PORTABLE stratum of the hot-reload bridge.

   Two complementary sources of facets:

   1. hive-schemas synthesizes conformance + relation facets straight from the
      malli value objects in hive-addon.hot.schema, so the schemas are claims
      under test rather than documentation.
   2. hive-test's deftrifecta carries golden + hand-authored mutation facets.
      Each mutant encodes a REAL regression of the cascade contract, so a
      surviving mutant names a blind spot rather than a style difference.

   Subject is hive-addon.hot.cascade/dependents: pure, host-free, and the
   function the whole bridge's correctness rests on — an under-broad closure
   leaves live addons holding stale sibling instances, an over-broad one
   remounts the world."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.hot.cascade :as cascade]
            [hive-addon.hot.schema :as hs]
            [hive-schemas.test :as hst]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Fixture graph — a -> b -> c (b depends on a, c depends on b)
;; =============================================================================

(defn- spec
  [id deps]
  (cond-> {:addon/id id
           :addon/type :native
           :addon/init-ns (str "probe." id)
           :addon/init-fn "make"}
    (seq deps) (assoc :addon/dependencies (set deps))))

(def chain
  [(spec "a" nil)
   (spec "b" ["a"])
   (spec "c" ["b"])])

;; Captured at load time, BEFORE any mutation rebinds the var root, so a mutant
;; can be expressed in terms of the real closure without recursing into itself.
(def ^:private real-dependents cascade/dependents)

(defn- sorted-ids [s] (vec (sort s)))

;; =============================================================================
;; Schema-synthesized facets (hive-schemas)
;; =============================================================================
;; :mutation false — the schema-derived mutant generator works off the REQUIRED
;; KEYS of a map-shaped :out. This subject returns a set, which yields no
;; mutants, and the mutants-present guard would fail loud. Mutation coverage for
;; this subject is hand-authored below instead, where the mutants can encode the
;; actual regressions.

(hst/deftrifecta-from-schema dependents-schema
  hive-addon.hot.cascade/dependents
  {:in  hs/DependentsArgs
   :out hs/AddonIdSet
   :mutation false
   :num-tests 100
   :rel (fn [[specs seeds] out]
          ;; FORWARD-CLOSURE, stated so that the degenerate answer fails it.
          ;; "contains its seeds" and "is a fixed point" are both satisfied by
          ;; out = seeds, so neither can see an under-broad closure — the exact
          ;; regression this subject exists to prevent. The load-bearing clause
          ;; is the third: anything depending on a member IS a member.
          ;; A declared dependency on an id that is NOT among the mounted specs
          ;; produces no edge and injects no sibling instance, so it must not
          ;; drag its declarer into the closure. Only a dependency that is
          ;; itself mounted counts.
          (let [mounted (set (map :addon/id specs))]
            (and (set? out)
                 (every? out seeds)
                 (every? (fn [s]
                           (or (not-any? #(and (mounted %) (out %))
                                         (:addon/dependencies s #{}))
                               (contains? out (:addon/id s))))
                         specs)
                 (= out (real-dependents specs out)))))})

;; =============================================================================
;; Golden + mutation facets (hive-test)
;; =============================================================================

(tri/deftrifecta dependents-closure
  hive-addon.hot.cascade/dependents
  {:golden-path "test/golden/hive-addon/dependents-closure.edn"
   :apply?      true
   :xf          sorted-ids
   :cases       {:seed-root   [chain #{"a"}]
                 :seed-middle [chain #{"b"}]
                 :seed-leaf   [chain #{"c"}]
                 :seed-split  [chain #{"a" "c"}]
                 :seed-absent [chain #{"nope"}]}
   :mutations
   [;; Reloads the changed addon and NOTHING else — the dependents keep their
    ;; stale sibling instances. The defect the cascade exists to prevent.
    ["seeds-only"
     (fn seeds-only
       ([_specs seeds] (set seeds))
       ([_specs seeds _opts] (set seeds)))]
    ;; Remounts the whole system for any seed — a leaf change tears down its
    ;; own dependencies.
    ["everything"
     (fn everything
       ([specs _seeds] (set (map :addon/id specs)))
       ([specs _seeds _opts] (set (map :addon/id specs))))]
    ;; Loses the seeds themselves; the changed addon is never rebuilt.
    ["drops-seeds"
     (fn drops-seeds
       ([specs seeds] (drops-seeds specs seeds {}))
       ([specs seeds opts]
        (reduce disj (real-dependents specs seeds opts) seeds)))]
    ;; Empty closure — reload becomes a silent no-op reporting success.
    ["empty"
     (fn empty-closure
       ([_specs _seeds] #{})
       ([_specs _seeds _opts] #{}))]]})

;; =============================================================================
;; Forward-closure property over REALISTIC graphs
;; =============================================================================
;; The schema-derived generator above draws :addon/id and the seed set
;; independently from [:string {:min 1}], so a generated seed almost never IS a
;; mounted id and the forward-closure clause passes VACUOUSLY. Measured: with the
;; transitive step deliberately removed, that facet stayed green.
;;
;; This generator builds an actual DAG — spec i may depend only on specs before
;; it — and draws the seeds FROM the mounted ids, so the clause is exercised on
;; every run. The two are complementary: the schema facet covers the pathological
;; universe (dangling deps, ids that were never mounted), this one covers the
;; universe the bridge actually runs in.

(def ^:private gen-graph-and-seeds
  (gen/let [n (gen/choose 1 6)]
    (let [ids (mapv #(str "n" %) (range n))]
      (gen/let [dep-idxs (apply gen/tuple
                                (for [i (range n)]
                                  (if (zero? i)
                                    (gen/return #{})
                                    (gen/set (gen/choose 0 (dec i))))))
                seed-idxs (gen/set (gen/choose 0 (dec n)) {:min-elements 1})]
        [(mapv (fn [i] (spec (ids i) (map ids (nth dep-idxs i)))) (range n))
         (set (map ids seed-idxs))]))))

(defspec dependents-is-forward-closed-over-real-graphs 200
  (prop/for-all [[specs seeds] gen-graph-and-seeds]
    (let [out     (cascade/dependents specs seeds)
          mounted (set (map :addon/id specs))]
      (and (set? out)
           ;; contains its seeds
           (every? out seeds)
           ;; forward-closed: a mounted dependency in the closure drags its
           ;; declarer in. This is the clause the deliberate break must trip.
           (every? (fn [s]
                     (or (not-any? #(and (mounted %) (out %))
                                   (:addon/dependencies s #{}))
                         (contains? out (:addon/id s))))
                   specs)
           ;; and it is a fixed point
           (= out (cascade/dependents specs out))))))

;; =============================================================================
;; Explicit contract assertions
;; =============================================================================

(deftest dependents-contract-holds-on-the-fixture-graph
  (testing "forward closure, seeds included"
    (is (= #{"a" "b" "c"} (cascade/dependents chain #{"a"})))
    (is (= #{"b" "c"} (cascade/dependents chain #{"b"})))
    (is (= #{"c"} (cascade/dependents chain #{"c"}))))
  (testing "a seed absent from specs is still returned — it is what changed"
    (is (= #{"nope"} (cascade/dependents chain #{"nope"}))))
  (testing "the closure is a fixed point"
    (doseq [seeds [#{"a"} #{"b"} #{"c"} #{"a" "c"}]]
      (let [out (cascade/dependents chain seeds)]
        (is (= out (cascade/dependents chain out))
            (str "closure not idempotent for " seeds))))))

(deftest port-outcome-schemas-accept-real-adapter-shapes
  (testing "TeardownOutcome"
    (is (hs/validate hs/TeardownOutcome {:torn-down ["c" "b" "a"] :errors []}))
    (is (hs/validate hs/TeardownOutcome {:torn-down []}))
    (is (not (hs/validate hs/TeardownOutcome {:torn-down "c"}))))
  (testing "NsReloadOutcome"
    (is (hs/validate hs/NsReloadOutcome {:loaded ['x.a 'x.b]}))
    (is (hs/validate hs/NsReloadOutcome {:loaded [] :failed 'x.b :error "boom"}))
    (is (not (hs/validate hs/NsReloadOutcome {:loaded nil})))))
