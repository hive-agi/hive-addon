(ns hive-addon.schema-edge-test
  "Edge-case coverage for validate-addon's error branches (surfaced by
   adversarial review): a method that THROWS a runtime error
   (:addon/method-threw), a REQUIRED method left unimplemented
   (:addon/contract-violation), and an extend-*-based addon that omits OPTIONAL
   methods — which on the JVM throws IllegalArgumentException rather than
   AbstractMethodError and must still be skipped, not misreported."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.protocol :as proto]
            [hive-addon.schema :as s]
            [hive-dsl.result :as r]))

(defn- throwing-addon []
  (reify proto/IAddon
    (addon-id [_] "throw.addon")
    (addon-type [_] :native)
    (capabilities [_] #{:tools})
    (initialize! [_ _cfg] {:success? true})
    (shutdown! [_] nil)
    (tools [_] [])
    (schema-extensions [_] [])
    (health [_] (throw (ex-info "boom" {:where :health})))
    (excluded-tools [_] #{})
    (hooks [_] {})))

(deftest method-threw-is-surfaced
  (testing "a pure method raising a non-contract exception -> :addon/method-threw"
    (let [res (s/validate-addon (throwing-addon))]
      (is (r/err? res))
      (is (= :addon/method-threw (:error res)))
      (is (= :addon/health (:method res))))))

;; reify omits `health` (a REQUIRED method) -> AbstractMethodError on call
(defn- missing-required-addon []
  (reify proto/IAddon
    (addon-id [_] "missing.addon")
    (addon-type [_] :native)
    (capabilities [_] #{:tools})
    (initialize! [_ _cfg] {:success? true})
    (shutdown! [_] nil)
    (tools [_] [])
    (schema-extensions [_] [])
    (excluded-tools [_] #{})
    (hooks [_] {})))

(deftest required-unimplemented-is-a-violation
  (testing "an unimplemented REQUIRED method -> :addon/contract-violation"
    (let [res (s/validate-addon (missing-required-addon))]
      (is (r/err? res))
      (is (= :addon/contract-violation (:error res)))
      (is (= :addon/health (:method res)))
      (is (= ["required protocol method not implemented"] (:explanation res))))))

;; extend-*-based addon: implements everything EXCEPT the two optional methods.
;; A missing extend-type method throws IllegalArgumentException ("No
;; implementation of method"), NOT AbstractMethodError — validate-addon must
;; recognize it as unimplemented and skip the optional methods.
(defrecord ExtendBasedAddon [])
(extend-type ExtendBasedAddon
  proto/IAddon
  (addon-id [_] "ext.addon")
  (addon-type [_] :external)
  (capabilities [_] #{:tools})
  (initialize! [_ _cfg] {:success? true})
  (shutdown! [_] nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok}))

(deftest extend-based-optional-omission-skips
  (testing "extend-* addon omitting optional methods conforms (IllegalArgumentException path)"
    (is (r/ok? (s/validate-addon (->ExtendBasedAddon))))))
