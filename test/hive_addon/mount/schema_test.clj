(ns hive-addon.mount.schema-test
  "Golden coverage for the mount value objects: MountSpec/MountPlan/MountResult/
   MountReport accept well-formed values and reject malformed ones; TeardownReport
   enforces the no-nuke invariant (:teardown/data-preserved? must be true)."
  (:require [clojure.test :refer [deftest is are testing]]
            [hive-addon.mount.schema :as ms]
            [hive-dsl.result :as r]))

(def ^:private a-spec
  {:addon/id      "a"
   :addon/type    :native
   :addon/init-ns "acme.a"
   :addon/init-fn "make"})

(def ^:private full-spec
  {:addon/id                     "b"
   :addon/type                   :native
   :addon/init-ns                "acme.b"
   :addon/init-fn                "make"
   :addon/kind                   :library
   :addon/version                "1.2.3"
   :addon/config                 {:port 42}
   :addon/capabilities           #{:cartography}
   :addon/dependencies           #{"a"}
   :addon/requires-capabilities  #{:vector-search}
   :addon/init-retry             {:max-attempts 3
                                  :initial-delay-ms 100
                                  :max-delay-ms 1000
                                  :backoff-factor 2}
   :addon/description            "the b addon"
   :addon/author                 nil
   :addon/license                "MIT"})

(deftest mount-spec-golden
  (testing "well-formed MountSpecs validate"
    (are [x] (true? (ms/validate ms/MountSpec x))
      a-spec
      full-spec))
  (testing "malformed MountSpecs are rejected"
    (are [x] (false? (ms/validate ms/MountSpec x))
      (dissoc a-spec :addon/id)
      (assoc a-spec :addon/id "")
      (assoc a-spec :addon/init-ns "")
      (dissoc a-spec :addon/init-fn)
      (assoc a-spec :addon/type :bogus)
      (assoc a-spec :addon/kind :nonsense)
      (assoc a-spec :addon/dependencies ["a"])          ; must be a set
      (assoc a-spec :addon/requires-capabilities #{"x"}) ; caps are keywords
      (assoc a-spec :addon/init-retry {:max-attempts 0})
      (assoc a-spec :addon/init-retry {:initial-delay-ms -1})
      (assoc a-spec :addon/init-retry {:backoff-factor 0})
      )))

(deftest mount-spec-registry-key
  (testing "MountSpec is reachable through the :mount/spec registry key"
    (is (true? (ms/validate :mount/spec a-spec)))
    (is (false? (ms/validate :mount/spec (dissoc a-spec :addon/id))))))

(deftest mount-plan-golden
  (testing "a well-formed MountPlan validates"
    (is (true? (ms/validate ms/MountPlan
                            {:ordered            [a-spec full-spec]
                             :cycles             #{}
                             :missing            {}
                             :unmet-capabilities {}
                             :duplicates         {}}))))
  (testing "populated diagnostics validate"
    (is (true? (ms/validate ms/MountPlan
                            {:ordered            []
                             :cycles             #{"a" "b"}
                             :missing            {"a" #{"z"}}
                             :unmet-capabilities {"b" #{:vector-search}}
                             :duplicates         {"a" 2}}))))
  (testing "malformed plans are rejected"
    (are [x] (false? (ms/validate ms/MountPlan x))
      {:cycles #{} :missing {} :unmet-capabilities {} :duplicates {}}   ; no :ordered
      {:ordered [{:not "a spec"}] :cycles #{} :missing {}
       :unmet-capabilities {} :duplicates {}}
      {:ordered [] :cycles ["a"] :missing {}
       :unmet-capabilities {} :duplicates {}}))) ; cycles must be a set

(deftest mount-result-golden
  (are [x] (true? (ms/validate ms/MountResult x))
    {:addon/id "a" :success? true :phase :initialized}
    {:addon/id "a" :success? true :phase :initialized :init-attempts 2}
    {:addon/id "a" :success? false :phase :failed :errors ["boom"]}
    {:addon/id "a" :success? true :phase :skipped :already-initialized? true})
  (are [x] (false? (ms/validate ms/MountResult x))
    {:addon/id "a" :success? true :phase :bogus}
    {:addon/id "a" :success? true :phase :initialized :init-attempts 0}
    {:addon/id "a" :phase :resolved}                    ; no :success?
    {:success? true :phase :resolved}))                 ; no :addon/id

(deftest mount-report-golden
  (is (true? (ms/validate ms/MountReport
                          {:mounted [{:addon/id "a" :success? true :phase :initialized}]
                           :order   ["a"]
                           :skipped #{}
                           :ok?     true})))
  (is (false? (ms/validate ms/MountReport
                           {:mounted [] :order ["a"] :skipped [] :ok? true})))) ; skipped must be a set

(deftest teardown-report-no-nuke
  (testing "a data-preserving teardown validates"
    (is (true? (ms/validate ms/TeardownReport
                            {:torn-down ["b" "a"] :teardown/data-preserved? true})))
    (is (true? (ms/validate ms/TeardownReport
                            {:torn-down [] :teardown/data-preserved? true
                             :errors ["shutdown of a threw"]}))))
  (testing "data-preserved? false is REJECTED — the no-nuke invariant"
    (is (false? (ms/validate ms/TeardownReport
                             {:torn-down [] :teardown/data-preserved? false})))
    (is (false? (ms/validate ms/TeardownReport
                             {:torn-down []})))))       ; flag required

(deftest validate*-result-bridge
  (testing "validate* bridges to a hive-dsl Result with a qualified category"
    (is (r/ok? (ms/validate* ms/MountSpec a-spec)))
    (let [e (ms/validate* ms/TeardownReport {:torn-down [] :teardown/data-preserved? false})]
      (is (r/err? e))
      (is (= :mount/schema-violation (:error e)))
      (is (some? (:explanation e))))
    (testing "custom category is honored"
      (is (= :mount/nope
             (:error (ms/validate* ms/MountSpec {} :mount/nope)))))))
