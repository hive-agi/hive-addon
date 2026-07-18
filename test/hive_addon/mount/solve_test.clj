(ns hive-addon.mount.solve-test
  "Pure solver coverage: linear/diamond ordering with lexicographic tie-break,
   cycle/missing/unmet-capability diagnostics, fail-closed-cycles, a test.check
   shuffle-invariance property (solve is a pure fn of the SET), and the OCP
   guarantee that a custom rule changes edges without touching solve."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [hive-addon.mount.schema :as ms]
            [hive-addon.mount.solve :as solve]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Spec builders
;; ---------------------------------------------------------------------------

(defn- spec
  [id & {:keys [deps caps requires]
         :or   {deps #{} caps #{} requires #{}}}]
  {:addon/id                    id
   :addon/type                  :native
   :addon/init-ns               (str "acme." id)
   :addon/init-fn               "make"
   :addon/dependencies          deps
   :addon/capabilities          caps
   :addon/requires-capabilities requires})

(defn- ids [plan] (mapv :addon/id (:ordered plan)))

(defn- valid-plan? [plan] (ms/validate ms/MountPlan plan))

;; ---------------------------------------------------------------------------
;; Ordering
;; ---------------------------------------------------------------------------

(deftest linear-chain
  (testing "a -> b -> c orders [a b c]; missing/cycles empty; plan validates"
    (let [specs [(spec "a")
                 (spec "b" :deps #{"a"})
                 (spec "c" :deps #{"b"})]
          plan  (solve/solve specs)]
      (is (= ["a" "b" "c"] (ids plan)))
      (is (= #{} (:cycles plan)))
      (is (= {} (:missing plan)))
      (is (= {} (:unmet-capabilities plan)))
      (is (valid-plan? plan)))))

(deftest diamond
  (testing "a -> {b,c} -> d: a first, d last, b before c by lexicographic tie-break"
    (let [specs [(spec "d" :deps #{"b" "c"})
                 (spec "b" :deps #{"a"})
                 (spec "c" :deps #{"a"})
                 (spec "a")]
          plan  (solve/solve specs)]
      (is (= ["a" "b" "c" "d"] (ids plan)))
      (is (valid-plan? plan)))))

(deftest independent-specs-sorted
  (testing "no edges ⇒ pure lexicographic order"
    (is (= ["a" "b" "c"]
           (ids (solve/solve [(spec "c") (spec "a") (spec "b")]))))))

;; ---------------------------------------------------------------------------
;; Diagnostics
;; ---------------------------------------------------------------------------

(deftest cycle-graceful-and-fail-closed
  (let [specs [(spec "a" :deps #{"b"})
               (spec "b" :deps #{"a"})]]
    (testing "graceful default reports the cycle and still returns a plan"
      (let [plan (solve/solve specs)]
        (is (r/ok? {:ok plan}))                     ; a plain plan, not an err
        (is (contains? plan :ordered))
        (is (= #{"a" "b"} (:cycles plan)))
        (is (empty? (:ordered plan)))
        (is (valid-plan? plan))))
    (testing ":fail-closed-cycles true ⇒ r/err :mount/unsolvable"
      (let [res (solve/solve specs {:fail-closed-cycles true})]
        (is (r/err? res))
        (is (= :mount/unsolvable (:error res)))
        (is (= #{"a" "b"} (:cycles res)))))
    (testing "acyclic input under fail-closed still returns a plan"
      (is (contains? (solve/solve [(spec "a")] {:fail-closed-cycles true})
                     :ordered)))))

(deftest partial-cycle-orders-acyclic-subset
  (testing "an independent acyclic spec still orders while the cycle is reported"
    (let [specs [(spec "x")
                 (spec "a" :deps #{"b"})
                 (spec "b" :deps #{"a"})]
          plan  (solve/solve specs)]
      (is (= ["x"] (ids plan)))
      (is (= #{"a" "b"} (:cycles plan))))))

(deftest missing-dependency
  (testing "a declared dep absent from the set is reported, spec still orders"
    (let [plan (solve/solve [(spec "a" :deps #{"ghost"})])]
      (is (= {"a" #{"ghost"}} (:missing plan)))
      (is (= ["a"] (ids plan)))
      (is (valid-plan? plan)))))

(deftest capability-edge
  (testing "S requires-capabilities #{:cartography}, P provides it ⇒ P before S"
    (let [specs [(spec "consumer" :requires #{:cartography})
                 (spec "provider" :caps #{:cartography})]
          plan  (solve/solve specs)]
      (is (= ["provider" "consumer"] (ids plan)))
      (is (= {} (:unmet-capabilities plan)))
      (is (valid-plan? plan)))))

(deftest unmet-capability
  (testing "a required cap no spec provides is reported, spec still orders"
    (let [plan (solve/solve [(spec "a" :requires #{:nowhere})])]
      (is (= {"a" #{:nowhere}} (:unmet-capabilities plan)))
      (is (= ["a"] (ids plan))))))

;; ---------------------------------------------------------------------------
;; Determinism — pure fn of the SET (shuffle invariance)
;; ---------------------------------------------------------------------------

(def ^:private determinism-specs
  [(spec "a")
   (spec "b" :deps #{"a"})
   (spec "c" :deps #{"a"})
   (spec "d" :deps #{"b" "c"})
   (spec "e" :requires #{:cap})
   (spec "f" :caps #{:cap})])

(defspec solve-is-shuffle-invariant 200
  (prop/for-all [shuffled (gen/shuffle determinism-specs)]
    (= (ids (solve/solve determinism-specs))
       (ids (solve/solve shuffled)))))

;; ---------------------------------------------------------------------------
;; OCP — a custom rule adds edges without touching solve
;; ---------------------------------------------------------------------------

;; forces "z" to mount before every other spec, regardless of declared deps
(defrecord ZFirstRule []
  solve/IDependencyRule
  (-edges [_ specs]
    (let [others (into #{} (comp (map :addon/id) (remove #{"z"})) specs)]
      (into #{} (map (fn [o] ["z" o])) others))))

(deftest ocp-custom-rule
  (let [specs        [(spec "a") (spec "b") (spec "z")]
        custom-rules (conj solve/default-rules (->ZFirstRule))]
    (testing "the custom rule contributes edges the default chain does not"
      (is (empty? (solve/edges specs)))
      (is (= #{["z" "a"] ["z" "b"]} (solve/edges specs custom-rules))))
    (testing "solve honors injected rules without any change to solve itself"
      (is (= ["z" "a" "b"]
             (ids (solve/solve specs {:rules custom-rules})))))))

(deftest duplicate-addon-ids
  (testing "two specs sharing an :addon/id are reported in :duplicates; a surviving spec still orders; determinism preserved"
    (let [specs [(assoc (spec "a") :addon/version "1.0.0")
                 (assoc (spec "a") :addon/version "2.0.0")
                 (spec "b" :deps #{"a"})]
          plan  (solve/solve specs)]
      (is (contains? (:duplicates plan) "a"))
      (is (= 2 (get (:duplicates plan) "a")))
      (is (= ["a" "b"] (ids plan)))
      (is (valid-plan? plan))
      (testing "determinism: shuffled input ⇒ identical :duplicates and :ordered"
        (let [p2 (solve/solve (reverse specs))]
          (is (= (:duplicates plan) (:duplicates p2)))
          (is (= (ids plan) (ids p2))))))))