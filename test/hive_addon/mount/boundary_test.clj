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

(use-fixtures :each
  (fn [t]
    (reset! ctor-configs {})
    (reset! shutdown-log [])
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
