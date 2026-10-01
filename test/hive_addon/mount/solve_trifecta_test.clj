(ns hive-addon.mount.solve-trifecta-test
  "Trifecta coverage for hive-addon.mount.solve/solve — the pure DAG solve every
   mount and every hot-reload cascade orders itself by.

   Why this subject, and why now: on cljrs the solver silently returned a
   LEXICOGRAPHIC order with `:cycles` empty, because an `:or` destructuring
   default bound the symbol `default-rules` instead of its value and the folded
   edge set came out empty. Nothing in the suite noticed — every existing
   assertion happened to use a fixture whose dependency order and alphabetical
   order coincide. The generative facets below cannot coincide by luck.

   TWO generators, deliberately, per the joint-distribution rule:

   1. hive-schemas synthesizes a facet from the malli value objects, drawing
      `:addon/id` and `:addon/dependencies` INDEPENDENTLY. That covers the
      pathological universe (dangling deps on ids that were never mounted) and
      is the only one that reaches it — but it essentially never produces an
      id that another spec actually depends on, so on its own every
      edge-sensitive clause would pass VACUOUSLY.
   2. A structured DAG generator that CONSTRUCTS the relation: n ids, spec i may
      depend only on j < i, then the specs are shuffled. That covers the
      universe the mounter actually runs in.

   `structured-generator-actually-produces-edges` is not ceremony: it is the
   guard that keeps facet 2 honest, and it is what a reviewer should look at
   first if these properties ever go green while the solver is broken."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.mount.schema :as ms]
            [hive-addon.mount.solve :as solve]
            [hive-schemas.test :as hst]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; Captured before any mutation rebinds the var root, so a mutant can be phrased
;; in terms of the real solve without recursing into itself.
(def ^:private real-solve solve/solve)

(defn- spec
  ([id] (spec id nil))
  ([id deps]
   (cond-> {:addon/id id
            :addon/type :native
            :addon/init-ns (str "probe." id)
            :addon/init-fn "make"}
     (seq deps) (assoc :addon/dependencies (set deps)))))

(defn- ordered-ids [plan] (mapv :addon/id (:ordered plan)))

(defn- position-of
  "id -> index within :ordered, or nil when the id was not ordered."
  [plan]
  (into {} (map-indexed (fn [i id] [id i])) (ordered-ids plan)))

(defn respects-every-edge?
  "Every ordering edge points FORWARD in :ordered.

   Edges are recomputed here from `solve/edges` rather than read out of the
   plan, so a solver that folded an empty edge set internally is caught: its
   order is judged against the edges that SHOULD have constrained it."
  [specs plan]
  (let [pos (position-of plan)]
    (every? (fn [[from to]]
              (let [i (pos from) j (pos to)]
                (or (nil? i) (nil? j) (< i j))))
            (solve/edges specs))))

;; =============================================================================
;; Facet 1 — schema-synthesized (hive-schemas)
;; =============================================================================
;; :mutation false — the schema-derived mutant generator works off the required
;; keys of a map-shaped :out and would mutate MountPlan's diagnostic keys, which
;; says nothing about ORDER. Mutation coverage is hand-authored below, where a
;; mutant can encode the regression that actually happened.

(hst/deftrifecta-from-schema solve-schema
  hive-addon.mount.solve/solve
  {:in  ms/SolveArgs
   :out ms/MountPlan
   :mutation false
   :num-tests 100
   :rel (fn [[specs] plan]
          (let [ids (set (map :addon/id specs))]
            (and
             ;; every ordered spec was an input spec
             (every? ids (ordered-ids plan))
             ;; :ordered and :cycles partition the id set — nothing invented,
             ;; nothing dropped without being accounted for as cyclic
             (= ids (into (set (ordered-ids plan)) (:cycles plan)))
             ;; no id appears twice in the order
             (= (count (ordered-ids plan)) (count (set (ordered-ids plan))))
             ;; and the order respects whatever edges this spec set implies
             (respects-every-edge? specs plan))))})

;; =============================================================================
;; Facet 2 — structured DAG generator (constructs the relation)
;; =============================================================================

(def gen-dag
  "A shuffled acyclic spec set whose dependencies REFERENCE the generated ids,
   and whose ids are LABELLED INDEPENDENTLY of their topological position.

   Three separate pieces of randomness, each load-bearing:

   1. Spec at topological index i may depend only on indices j < i, which makes
      acyclicity structural rather than something the generator retries into.
   2. The names are a SHUFFLED pool, so index i's id is uncorrelated with i.
      Without this the ids read n0, n1, n2... in dependency order and
      alphabetical order IS a valid topological order — a solver that ignores
      every edge and just sorts by id then satisfies the order property on every
      sample. Measured: the naive labelling passed 200/200 against exactly the
      regression this file exists to catch.
   3. The spec vector is shuffled, so :ordered is evidence about the solve
      rather than about input order.

   The name pool is always the full n0..n(n-1), permuted — only the assignment
   moves — so gen-cyclic can still close a cycle by naming \"n0\" and \"n1\"."
  (gen/let [n (gen/choose 2 7)]
    (gen/let [names (gen/shuffle (mapv #(str "n" %) (range n)))]
      (gen/let [deps (apply gen/tuple
                            (map (fn [i]
                                   (if (zero? i)
                                     (gen/return #{})
                                     (gen/set (gen/elements (subvec names 0 i)))))
                                 (range n)))
                order (gen/shuffle (vec (range n)))]
        (mapv (fn [i] (spec (nth names i) (nth deps i))) order)))))

(defspec structured-order-respects-every-edge 200
  (prop/for-all [specs gen-dag]
    (let [plan (solve/solve specs)]
      (and (empty? (:cycles plan))
           (= (count specs) (count (:ordered plan)))
           (respects-every-edge? specs plan)))))

(defspec structured-solve-is-order-independent 100
  (prop/for-all [specs gen-dag]
    ;; A pure fn of the spec SET: shuffling the input cannot move the output.
    (= (ordered-ids (solve/solve specs))
       (ordered-ids (solve/solve (shuffle specs))))))

(def gen-cyclic
  "A DAG plus a guaranteed 2-cycle between \"n0\" and \"n1\".

   The cycle is introduced BY ID, never by position: gen-dag shuffles its output,
   so \"first and last of the vector\" names two arbitrary specs and the back-edge
   it adds often closes nothing. gen-dag always names its ids n0..n(n-1) and
   always produces at least two, so making n0 and n1 depend on each other is a
   cycle by construction rather than by luck."
  (gen/let [specs gen-dag]
    (mapv (fn [s]
            (case (:addon/id s)
              "n0" (update s :addon/dependencies (fnil conj #{}) "n1")
              "n1" (update s :addon/dependencies (fnil conj #{}) "n0")
              s))
          specs)))

(defspec structured-cycles-are-detected-and-excluded 100
  (prop/for-all [specs gen-cyclic]
    (let [plan (solve/solve specs)
          ord  (set (ordered-ids plan))]
      (and (seq (:cycles plan))
           ;; a cyclic id is never ordered
           (empty? (clojure.set/intersection ord (set (:cycles plan))))
           ;; and whatever WAS ordered is still correctly ordered
           (respects-every-edge? specs plan)))))

;; =============================================================================
;; The non-vacuity guard
;; =============================================================================

(deftest structured-generator-actually-produces-edges
  (testing "the DAG generator constructs real dependency edges, so the
            edge-respecting properties above are not tautologies"
    (let [samples     (gen/sample gen-dag 100)
          edge-counts (map (fn [specs] (count (solve/edges specs))) samples)
          with-edges  (count (filter pos? edge-counts))]
      (is (> with-edges 50)
          (str "only " with-edges "/100 generated spec sets had any edge at all; "
               "the order properties would be passing vacuously"))
      (is (pos? (apply max edge-counts))
          "no sample produced a single edge")))

  (testing "and the ids are labelled independently of topological position, so
            sorting by id is NOT a valid answer"
    ;; The sharper vacuity guard. A generator whose ids happen to ascend with
    ;; dependency order lets a solver that ignores every edge and merely sorts
    ;; satisfy the order property on every sample — measured at 200/200 before
    ;; the name pool was shuffled. This asserts the samples can actually tell
    ;; the two apart.
    (let [sort-by-id  (fn [specs]
                        (let [by-id (into {} (map (juxt :addon/id identity)) (set specs))]
                          {:ordered (mapv by-id (sort (keys by-id)))}))
          samples     (gen/sample gen-dag 100)
          discriminating (count (remove (fn [specs]
                                          (respects-every-edge? specs (sort-by-id specs)))
                                        samples))]
      (is (pos? discriminating)
          (str "in 100 samples, sorting by :addon/id was ALWAYS a valid topological "
               "order — the generator cannot distinguish a real solve from a sort")))))

(deftest schema-generator-reaches-the-pathological-universe
  (testing "the schema-derived generator draws ids and dependencies
            independently, which is what lets it reach dangling dependencies —
            the case the structured generator can never produce"
    (let [plan (solve/solve [(spec "only" ["never-mounted"])])]
      (is (= ["only"] (ordered-ids plan)))
      (is (= {"only" #{"never-mounted"}} (:missing plan))
          "a dangling dependency is diagnosed, not silently dropped")
      (is (empty? (:cycles plan))))))

;; =============================================================================
;; Golden + mutation facets (hive-test)
;; =============================================================================

(def diamond
  "a -> {b, c} -> d. Alphabetical order and dependency order COINCIDE here, which
   is exactly why the golden alone could not catch the cljrs regression; the
   mutants below are what make this fixture discriminating."
  [(spec "d" ["b" "c"])
   (spec "b" ["a"])
   (spec "c" ["a"])
   (spec "a")])

(def reverse-alpha
  "Dependency order is the REVERSE of alphabetical order, so a solver that sorts
   instead of topo-sorting cannot pass by coincidence."
  [(spec "a" ["b"])
   (spec "b" ["c"])
   (spec "c")])

(tri/deftrifecta solve-order
  hive-addon.mount.solve/solve
  {:golden-path "test/golden/hive-addon/solve-order.edn"
   :apply?      true
   :xf          (fn [plan]
                  {:ordered (mapv :addon/id (:ordered plan))
                   :cycles  (vec (sort (:cycles plan)))
                   :missing (into (sorted-map)
                                  (map (fn [[k v]] [k (vec (sort v))]))
                                  (:missing plan))})
   :cases       {:diamond       [diamond]
                 :reverse-alpha [reverse-alpha]
                 :cyclic        [[(spec "x" ["y"]) (spec "y" ["x"]) (spec "free")]]
                 :capability    [[{:addon/id "consumer" :addon/type :native
                                   :addon/init-ns "probe.consumer" :addon/init-fn "make"
                                   :addon/requires-capabilities #{:store}}
                                  {:addon/id "provider" :addon/type :native
                                   :addon/init-ns "probe.provider" :addon/init-fn "make"
                                   :addon/capabilities #{:store}}]]
                 :dangling      [[(spec "only" ["never-mounted"])]]}
   :mutations
   [;; THE regression that actually shipped on cljrs: an empty edge set, so the
    ;; Kahn sort degenerates to the lexicographic tie-break and reports no
    ;; cycles. Passes any fixture whose dependency order is alphabetical.
    ["lexicographic-no-edges"
     (fn lexicographic
       ([specs] (lexicographic specs {}))
       ([specs _opts]
        (let [by-id (into {} (map (juxt :addon/id identity)) (set specs))]
          {:ordered (mapv by-id (sort (keys by-id)))
           :cycles #{} :missing {} :unmet-capabilities {} :duplicates {}})))]
    ;; Orders dependents BEFORE their dependencies — every addon receives a
    ;; sibling that has not been constructed yet.
    ["reversed"
     (fn reversed
       ([specs] (reversed specs {}))
       ([specs opts] (update (real-solve specs opts) :ordered (comp vec reverse))))]
    ;; Detects no cycles and orders the cyclic nodes anyway.
    ["cycles-ignored"
     (fn cycles-ignored
       ([specs] (cycles-ignored specs {}))
       ([specs opts]
        (let [plan (real-solve specs opts)
              by-id (into {} (map (juxt :addon/id identity)) (set specs))]
          (assoc plan
                 :cycles #{}
                 :ordered (into (vec (:ordered plan))
                                (map by-id (sort (:cycles plan))))))))]
    ;; Drops the diagnostics — a dangling dependency stops being reported.
    ["silent-diagnostics"
     (fn silent-diagnostics
       ([specs] (silent-diagnostics specs {}))
       ([specs opts] (assoc (real-solve specs opts) :missing {} :unmet-capabilities {})))]]})
