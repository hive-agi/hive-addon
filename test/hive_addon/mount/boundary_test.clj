(ns hive-addon.mount.boundary-test
  "Effectful-boundary coverage with an in-memory atom-mount-host and reify fake
   IAddon instances (NO live systems): mount order + DIP sibling injection,
   graceful degrade (failing/non-IAddon/throwing ctor + throwing initialize! are
   recorded, later specs still mount, succeeded ones are NOT torn down), dry-run
   golden-replay parity, and reverse-order teardown with the no-nuke invariant."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.port :as port]
            [hive-addon.mount.schema :as ms]
            [hive-addon.mount.solve :as solve]
            [hive-addon.protocol :as proto]))

;; ---------------------------------------------------------------------------
;; Observation atoms — reset per test
;; ---------------------------------------------------------------------------

(def ctor-configs   (atom {}))   ; id -> config the ctor received (sibling-injection probe)
(def shutdown-log   (atom []))   ; ids shut down, in call order
(def init-calls     (atom {}))

(use-fixtures :each
  (fn [t]
    (reset! ctor-configs {})
    (reset! shutdown-log [])
    (reset! init-calls {})
    (t)))

;; ---------------------------------------------------------------------------
;; Fake IAddon constructors — resolved by requiring-resolve from MountSpecs
;; ---------------------------------------------------------------------------

(defn- fake-addon
  [id config]
  (reify proto/IAddon
    (addon-id [_] id)
    (addon-type [_] :native)
    (capabilities [_] (:__caps config #{}))
    (initialize! [_ _cfg] {:success? true})
    (shutdown! [_] (swap! shutdown-log conj id) nil)
    (tools [_] [])
    (schema-extensions [_] [])
    (health [_] {:status :ok})
    (excluded-tools [_] #{})
    (hooks [_] {})))

(defn make-ok-addon
  "Well-behaved fake addon ctor; records the config it received."
  [config]
  (let [id (:addon/id config)]
    (swap! ctor-configs assoc id config)
    (fake-addon id config)))

(defn make-non-addon
  "Ctor that returns a plain map — not an IAddon."
  [_config]
  {:not :an-addon})

(defn make-nil-addon
  "Ctor that returns nil (a failing ctor)."
  [_config]
  nil)

(defn make-throwing-ctor
  "Ctor that throws."
  [_config]
  (throw (ex-info "boom in ctor" {})))

(defn make-init-throws
  "Ctor returning an addon whose initialize! throws."
  [config]
  (let [id (:addon/id config)]
    (swap! ctor-configs assoc id config)
    (reify proto/IAddon
      (addon-id [_] id)
      (addon-type [_] :native)
      (capabilities [_] #{})
      (initialize! [_ _cfg] (throw (ex-info "boom in init" {})))
      (shutdown! [_] (swap! shutdown-log conj id) nil)
      (tools [_] [])
      (schema-extensions [_] [])
      (health [_] {:status :ok})
      (excluded-tools [_] #{})
      (hooks [_] {}))))

(defn make-flaky-init
  "Ctor whose initializer succeeds on the configured attempt."
  [config]
  (let [id (:addon/id config)
        succeed-at (:__succeed-at config Long/MAX_VALUE)]
    (reify proto/IAddon
      (addon-id [_] id)
      (addon-type [_] :native)
      (capabilities [_] #{})
      (initialize! [_ _cfg]
        (let [attempt (get (swap! init-calls update id (fnil inc 0)) id)]
          (if (>= attempt succeed-at)
            {:success? true}
            {:success? false :errors [(str "not ready " attempt)]})))
      (shutdown! [_] nil)
      (tools [_] [])
      (schema-extensions [_] [])
      (health [_] {:status :ok})
      (excluded-tools [_] #{})
      (hooks [_] {}))))

;; ---------------------------------------------------------------------------
;; Spec builder — :addon/config carries :addon/id so the default resolver works
;; ---------------------------------------------------------------------------

(defn- spec
  [id ctor & {:keys [deps caps requires]
              :or   {deps #{} caps #{} requires #{}}}]
  {:addon/id                    id
   :addon/type                  :native
   :addon/init-ns               "hive-addon.mount.boundary-test"
   :addon/init-fn               ctor
   :addon/dependencies          deps
   :addon/capabilities          caps
   :addon/requires-capabilities requires
   :addon/config                (cond-> {:addon/id id}
                                  (seq caps) (assoc :__caps caps))})

(defn- result-for [report id]
  (first (filter #(= id (:addon/id %)) (:mounted report))))

(defn- throwing-registered-host
  "An IMountHost whose `registered` always throws; register!/init!/shutdown!
   work against a backing atom (shutdown! records into shutdown-log)."
  []
  (let [reg (atom {})]
    (reify port/IMountHost
      (register! [_ addon] (swap! reg assoc (proto/addon-id addon) addon) nil)
      (init! [_ addon-id config]
        (proto/initialize! (get @reg addon-id) config))
      (shutdown! [_ addon-id] (swap! shutdown-log conj addon-id) nil)
      (registered [_ _addon-id] (throw (ex-info "registered boom" {}))))))

;; ---------------------------------------------------------------------------
;; mount! — order + DIP sibling injection
;; ---------------------------------------------------------------------------

(deftest mount-order-and-sibling-injection
  (testing "a -> b mounts in order; b's config carries the a instance under :mount/dependencies"
    (let [host   (port/atom-mount-host)
          specs  [(spec "a" "make-ok-addon")
                  (spec "b" "make-ok-addon" :deps #{"a"})]
          plan   (solve/solve specs)
          report (boundary/mount! plan host)]
      (is (= ["a" "b"] (:order report)))
      (is (:ok? report))
      (is (= #{} (:skipped report)))
      (is (every? :success? (:mounted report)))
      (is (ms/validate ms/MountReport report))
      (testing "sibling injection: b received {:mount/dependencies {\"a\" <a-instance>}}"
        (let [a-inst (port/registered host "a")
              b-cfg  (get @ctor-configs "b")]
          (is (some? a-inst))
          (is (= {"a" a-inst} (:mount/dependencies b-cfg))))))))

(deftest capability-sibling-injection
  (testing "a capability provider is injected into the requiring addon's config"
    (let [host   (port/atom-mount-host)
          specs  [(spec "consumer" "make-ok-addon" :requires #{:cartography})
                  (spec "provider" "make-ok-addon" :caps #{:cartography})]
          plan   (solve/solve specs)
          _      (boundary/mount! plan host)
          p-inst (port/registered host "provider")]
      (is (= {"provider" p-inst}
             (:mount/dependencies (get @ctor-configs "consumer")))))))

;; ---------------------------------------------------------------------------
;; Graceful degrade — failures recorded, later specs still mount, no teardown
;; ---------------------------------------------------------------------------

(deftest graceful-degrade-failing-ctor
  (testing "nil ctor / non-IAddon / throwing ctor / throwing init are recorded; later specs still mount; NO teardown of succeeded ones"
    (let [host   (port/atom-mount-host)
          specs  [(spec "a-unres"  "make-does-not-exist") ; unresolvable ctor -> :resolved
                  (spec "a-nil"    "make-nil-addon")       ; resolves, returns nil -> :failed
                  (spec "b-nonad"  "make-non-addon")
                  (spec "c-throw"  "make-throwing-ctor")
                  (spec "d-initx"  "make-init-throws")
                  (spec "e-ok"     "make-ok-addon")
                  (spec "f-ok"     "make-ok-addon")]
          plan   (solve/solve specs)
          report (boundary/mount! plan host)]
      (is (false? (:ok? report)))
      (is (false? (:success? (result-for report "a-unres"))))
      (is (= :resolved (:phase (result-for report "a-unres"))))
      (is (false? (:success? (result-for report "a-nil"))))
      (is (= :failed (:phase (result-for report "a-nil"))))
      (is (false? (:success? (result-for report "b-nonad"))))
      (is (= :failed (:phase (result-for report "b-nonad"))))
      (is (false? (:success? (result-for report "c-throw"))))
      (is (= :failed (:phase (result-for report "c-throw"))))
      (is (false? (:success? (result-for report "d-initx"))))
      (is (= :initialized (:phase (result-for report "d-initx"))))
      (testing "the two well-behaved specs mounted despite earlier failures"
        (is (:success? (result-for report "e-ok")))
        (is (:success? (result-for report "f-ok")))
        (is (some? (port/registered host "e-ok")))
        (is (some? (port/registered host "f-ok"))))
      (testing "graceful degrade never tears down succeeded addons mid-DAG"
        (is (= [] @shutdown-log)))
      (is (contains? (:skipped report) "a-nil"))
      (is (ms/validate ms/MountReport report)))))

(deftest init-retries-with-backoff-and-events
  (let [host (port/atom-mount-host)
        sleeps (atom [])
        events (atom [])
        retry {:max-attempts 4
               :initial-delay-ms 10
               :max-delay-ms 100
               :backoff-factor 2}
        flaky (-> (spec "flaky" "make-flaky-init")
                  (assoc :addon/init-retry retry)
                  (assoc-in [:addon/config :__succeed-at] 3))
        report (boundary/mount! (solve/solve [flaky]) host
                                {:sleep-fn #(swap! sleeps conj %)
                                 :on-event #(swap! events conj %)})
        result (result-for report "flaky")]
    (is (:ok? report))
    (is (:success? result))
    (is (= 3 (:init-attempts result)))
    (is (= 3 (get @init-calls "flaky")))
    (is (= [10 20] @sleeps))
    (is (= [:mount/init-attempt
            :mount/init-retry
            :mount/init-attempt
            :mount/init-retry
            :mount/init-attempt
            :mount/init-succeeded]
           (mapv :event @events)))
    (is (ms/validate ms/MountReport report))))

(deftest init-retry-exhaustion-reports-last-attempt
  (let [host (port/atom-mount-host)
        flaky (-> (spec "flaky" "make-flaky-init")
                  (assoc :addon/init-retry {:max-attempts 3
                                            :initial-delay-ms 0
                                            :max-delay-ms 0
                                            :backoff-factor 2}))
        report (boundary/mount! (solve/solve [flaky]) host)
        result (result-for report "flaky")]
    (is (false? (:ok? report)))
    (is (= 3 (:init-attempts result)))
    (is (= 3 (get @init-calls "flaky")))
    (is (= ["not ready 3"] (:errors result)))
    (is (contains? (:skipped report) "flaky"))))

(deftest init-default-remains-single-attempt
  (let [host (port/atom-mount-host)
        report (boundary/mount! (solve/solve [(spec "flaky" "make-flaky-init")]) host)
        result (result-for report "flaky")]
    (is (false? (:success? result)))
    (is (= 1 (:init-attempts result)))
    (is (= 1 (get @init-calls "flaky")))))

(deftest event-sink-failure-does-not-break-mount
  (let [host (port/atom-mount-host)
        report (boundary/mount! (solve/solve [(spec "ok" "make-ok-addon")]) host
                                {:on-event (fn [_] (throw (ex-info "logger down" {})))})]
    (is (:ok? report))))

(deftest invalid-retry-policy-is-a-config-failure
  (let [host (port/atom-mount-host)
        bad (assoc (spec "bad" "make-ok-addon")
                   :addon/init-retry {:max-attempts 0})
        report (boundary/mount! (solve/solve [bad]) host)
        result (result-for report "bad")]
    (is (false? (:success? result)))
    (is (= :config (:phase result)))
    (is (re-find #"max-attempts" (first (:errors result))))))

;; ---------------------------------------------------------------------------
;; dry-run golden-replay parity
;; ---------------------------------------------------------------------------

(deftest dry-run-parity
  (testing "for an all-success plan, dry-run :order and :mounted ids equal mount! :order and :mounted ids; dry-run performs no effects"
    (let [specs    [(spec "a" "make-ok-addon")
                    (spec "b" "make-ok-addon" :deps #{"a"})
                    (spec "c" "make-ok-addon" :deps #{"b"})]
          plan     (solve/solve specs)
          dry-host (port/atom-mount-host)
          dry      (boundary/dry-run plan dry-host)]
      (testing "dry-run mutated nothing"
        (is (nil? (port/registered dry-host "a")))
        (is (= {} @ctor-configs))
        (is (:ok? dry)))
      (let [wet-host (port/atom-mount-host)
            wet      (boundary/mount! plan wet-host)]
        (is (= (:order wet) (:order dry)))
        (is (= (mapv :addon/id (:mounted wet))
               (mapv :addon/id (:mounted dry))))
        (is (ms/validate ms/MountReport dry))))))

;; ---------------------------------------------------------------------------
;; teardown! — reverse order, no-nuke invariant
;; ---------------------------------------------------------------------------

(deftest teardown-reverse-order
  (testing "teardown! shuts down in reverse mount order; :teardown/data-preserved? true"
    (let [host   (port/atom-mount-host)
          specs  [(spec "a" "make-ok-addon")
                  (spec "b" "make-ok-addon" :deps #{"a"})
                  (spec "c" "make-ok-addon" :deps #{"b"})]
          plan   (solve/solve specs)
          report (boundary/mount! plan host)
          td     (boundary/teardown! host (:order report))]
      (is (= ["c" "b" "a"] (:torn-down td)))
      (is (= ["c" "b" "a"] @shutdown-log))
      (is (true? (:teardown/data-preserved? td)))
      (is (ms/validate ms/TeardownReport td))
      (testing "TeardownReport rejects :teardown/data-preserved? false (no-nuke)"
        (is (not (ms/validate ms/TeardownReport (assoc td :teardown/data-preserved? false))))))))

(deftest graceful-degrade-throwing-resolve-config
  (testing "an injected resolve-config that throws for one spec is caught per-spec: earlier and later specs still mount, no teardown occurs"
    (let [host   (port/atom-mount-host)
          specs  [(spec "a" "make-ok-addon")
                  (spec "b" "make-ok-addon" :deps #{"a"})
                  (spec "c" "make-ok-addon" :deps #{"b"})]
          plan   (solve/solve specs)
          rc     (fn [spec]
                   (if (= "b" (:addon/id spec))
                     (throw (ex-info "DI fail" {}))
                     (:addon/config spec {})))
          report (boundary/mount! plan host {:resolve-config rc})]
      (is (= ["a" "b" "c"] (:order report)))
      (is (false? (:ok? report)))
      (testing "a mounted"
        (is (:success? (result-for report "a")))
        (is (some? (port/registered host "a"))))
      (testing "b recorded failed at the config seam, not thrown"
        (is (false? (:success? (result-for report "b"))))
        (is (= :config (:phase (result-for report "b"))))
        (is (nil? (port/registered host "b"))))
      (testing "c STILL mounts after the DI-seam failure"
        (is (:success? (result-for report "c")))
        (is (some? (port/registered host "c"))))
      (testing "no teardown of already-mounted addons"
        (is (= [] @shutdown-log)))
      (is (contains? (:skipped report) "b"))
      (is (ms/validate ms/MountReport report)))))

(deftest graceful-degrade-throwing-registered
  (testing "a custom IMountHost whose `registered` throws is caught per-spec: independent specs still mount, no teardown occurs"
    (let [host   (throwing-registered-host)
          specs  [(spec "a" "make-ok-addon")
                  (spec "b" "make-ok-addon" :deps #{"a"})
                  (spec "d" "make-ok-addon")]
          plan   (solve/solve specs)
          report (boundary/mount! plan host)]
      (is (false? (:ok? report)))
      (testing "a mounted (no dep lookup, so registered never called)"
        (is (:success? (result-for report "a"))))
      (testing "b recorded failed at the config seam when registered threw"
        (is (false? (:success? (result-for report "b"))))
        (is (= :config (:phase (result-for report "b")))))
      (testing "the independent d STILL mounts despite the throwing sibling lookup"
        (is (:success? (result-for report "d"))))
      (testing "the throwing sibling lookup did not abort mount! or tear anything down"
        (is (= [] @shutdown-log)))
      (is (ms/validate ms/MountReport report)))))
