(ns hive-addon.diagnostic-test
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.diagnostic :as diagnostic]
            [hive-addon.hot :as hot]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.port :as port]))

(deftest absent-addon-reload-has-recovery-without-effects
  (let [report (hot/reload-addon! nil [] "missing")]
    (is (false? (:ok? report)))
    (is (= :addon/not-mounted (get-in report [:diagnostic :code])))
    (is (true? (:teardown/data-preserved? report)))
    (is (empty? (:hot/torn-down report)))
    (is (.contains (get-in report [:diagnostic :message]) "hot inject"))))

(deftest namespace-diagnostic-survives-addon-strategy
  (let [problem {:code :hot/reload-failed :message "fix fixture" :retryable false :actions []}
        spec {:addon/id "fixture" :addon/type :native
              :addon/init-ns "hive-addon.diagnostic-test"
              :addon/init-fn "must-not-run"
              :addon/dependencies #{} :addon/requires-capabilities #{}}
        report (hot/reload-addon!
                (port/atom-mount-host) [spec] "fixture"
                {:reload-ns! (fn [_] {:failed (symbol "fixture.broken")
                                     :loaded [] :diagnostic problem})})]
    (is (false? (:ok? report)))
    (is (= problem (:diagnostic report)))
    (is (empty? (:hot/torn-down report)))
    (is (empty? (:mounted report)))))

(deftest unresolved-constructor-is-an-actionable-mount-failure
  (let [host (port/atom-mount-host)
        report (boundary/mount!
                {:ordered [{:addon/id "missing" :addon/type :native
                            :addon/init-ns "hive.diagnostic.nonexistent"
                            :addon/init-fn "create"
                            :addon/dependencies #{}
                            :addon/requires-capabilities #{}}]} host)
        result (first (:mounted report))]
    (is (false? (:ok? report)))
    (is (false? (:success? result)))
    (is (= :addon.mount/resolved (get-in result [:diagnostic :code])))
    (is (seq (:errors result)))
    (is (.contains (get-in result [:diagnostic :message]) "classpath"))))

(deftest diagnosis-preserves-domain-evidence
  (doseq [phase (keys diagnostic/mount-guidance)]
    (let [result {:addon/id "fixture" :success? false :phase phase
                  :errors ["original"] :init-attempts 2}
          enriched (diagnostic/mount-outcome result)]
      (is (= result (dissoc enriched :diagnostic)))
      (is (false? (get-in enriched [:diagnostic :retryable])))))
  (let [success {:addon/id "ok" :success? true :phase :initialized}]
    (is (identical? success (diagnostic/mount-outcome success)))))
