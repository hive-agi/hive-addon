(ns hive-addon.mount.entitlement-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.entitlement :as ent]
            [hive-addon.mount.port :as port]
            [hive-addon.mount.schema :as ms]
            [hive-addon.protocol :as proto]))

(defrecord StubAddon [id]
  proto/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] #{})
  (initialize! [_ _] {:success? true :errors []})
  (shutdown! [_] nil)
  (tools [_] [])
  (excluded-tools [_] #{})
  (hooks [_] {})
  (schema-extensions [_] [])
  (health [_] {:status :healthy}))

(defn stub-ctor [_config] (->StubAddon "premium.addon"))

(defn free-ctor [_config] (->StubAddon "free.addon"))

(defn- spec
  ([] (spec {}))
  ([overrides]
   (merge {:addon/id "premium.addon"
           :addon/type :native
           :addon/init-ns "hive-addon.mount.entitlement-test"
           :addon/init-fn "stub-ctor"}
          overrides)))

(def ^:private free-spec (spec))
(def ^:private paid-spec (spec {:addon/trust-class :proprietary
                                :addon/entitlement "io.github.hive-agi:hive-carto"}))

(defn- plan [& specs] {:ordered (vec specs)})

(defn- with-closed-gate [f]
  (ent/reset-gate!)
  (f)
  (ent/reset-gate!))

(use-fixtures :each with-closed-gate)

(deftest specs-are-gated-only-when-proprietary
  (is (not (ent/gated? free-spec)))
  (is (ent/gated? paid-spec))
  (testing "an absent trust-class is free"
    (is (not (ent/gated? (spec {}))))))

(deftest both-schemas-still-validate
  (is (ms/validate ms/MountSpec free-spec))
  (is (ms/validate ms/MountSpec paid-spec)))

(deftest the-default-posture-is-closed
  (testing "an unconfigured host refuses a proprietary addon"
    (is (= :deny/no-license-gate (ent/permit (ent/installed-gate) paid-spec))))
  (testing "and still mounts a free one"
    (is (nil? (ent/permit (ent/installed-gate) free-spec)))))

(deftest a-gate-can-be-a-plain-function
  (testing "no protocol import needed to supply licence behaviour"
    (is (= :deny/expired (ent/permit (fn [_] :deny/expired) paid-spec)))
    (is (nil? (ent/permit (fn [_] nil) paid-spec)))))

(deftest a-gate-never-sees-an-ungated-spec
  (let [seen (atom [])]
    (ent/permit (fn [s] (swap! seen conj s) nil) free-spec)
    (is (empty? @seen))))

(deftest a-throwing-gate-refuses-rather-than-escapes
  (is (= :deny/gate-error
         (ent/permit (fn [_] (throw (ex-info "boom" {}))) paid-spec))))

(deftest installing-a-gate-is-the-swap-point
  (ent/install-gate! ent/open-gate)
  (is (nil? (ent/permit (ent/installed-gate) paid-spec)))
  (ent/reset-gate!)
  (is (= :deny/no-license-gate (ent/permit (ent/installed-gate) paid-spec))))

(deftest a-refused-spec-does-not-mount
  (let [host   (port/atom-mount-host)
        report (boundary/mount! (plan paid-spec) host {})
        result (first (:mounted report))]
    (is (false? (:ok? report)))
    (is (= :entitlement (:phase result)))
    (is (= :deny/no-license-gate (:deny/reason result)))
    (is (nil? (port/registered host "premium.addon"))
        "a refused addon must never reach the registry")
    (is (ms/validate ms/MountResult result))))

(deftest a-permitted-spec-mounts
  (let [host   (port/atom-mount-host)
        report (boundary/mount! (plan paid-spec) host {:license-gate ent/open-gate})]
    (is (:ok? report))
    (is (= :initialized (:phase (first (:mounted report)))))
    (is (some? (port/registered host "premium.addon")))))

(deftest refusal-precedes-constructor-resolution
  (testing "unlicensed code is not merely unused, it is never loaded"
    (let [host   (port/atom-mount-host)
          broken (assoc paid-spec :addon/init-ns "no.such.namespace.at.all")
          report (boundary/mount! (plan broken) host {})
          result (first (:mounted report))]
      (is (= :entitlement (:phase result))
          "a missing namespace must not be reported: the gate ran first"))))

(deftest one-refusal-does-not-stop-the-others
  (testing "graceful degrade still holds with the gate in the loop"
    (let [host   (port/atom-mount-host)
          free   (spec {:addon/id "free.addon" :addon/init-fn "free-ctor"})
          report (boundary/mount! (plan paid-spec free) host {})]
      (is (false? (:ok? report)))
      (is (= #{"premium.addon"} (:skipped report)))
      (is (some? (port/registered host "free.addon"))))))

(deftest dry-run-agrees-with-mount
  (testing "the gate is visible without effects"
    (let [host (port/atom-mount-host)
          dry  (boundary/dry-run (plan paid-spec) host {})]
      (is (= :entitlement (:phase (first (:mounted dry)))))
      (is (= :deny/no-license-gate (:deny/reason (first (:mounted dry)))))
      (is (nil? (port/registered host "premium.addon"))))
    (let [host (port/atom-mount-host)
          dry  (boundary/dry-run (plan paid-spec) host {:license-gate ent/open-gate})]
      (is (:ok? dry))
      (is (= :skipped (:phase (first (:mounted dry))))))))

(deftest the-gate-receives-the-entitlement-unit
  (testing "a gate decides per unit, not per addon id"
    (let [seen (atom nil)]
      (boundary/dry-run (plan paid-spec) (port/atom-mount-host)
                        {:license-gate (fn [s] (reset! seen (:addon/entitlement s)) nil)})
      (is (= "io.github.hive-agi:hive-carto" @seen)))))
