(ns hive-addon.opaque.manifest-test
  "hive-addon.opaque/->manifest takes trust-class and entitlement from the
   OpaqueSpec, and the licence gate judges the result."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.mount.entitlement :as ent]
            [hive-addon.mount.schema :as ms]
            [hive-addon.opaque :as opaque]
            [hive-addon.opaque.schema :as os]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private plain-spec
  {:opaque/exec "/opt/dev/my-wasm-kernel"
   :opaque/id   "dev.wasm-kernel"})

(def ^:private paid-spec
  (assoc plain-spec :opaque/trust-class :proprietary))

(def ^:private refusing-gate
  (reify ent/ILicenseGate
    (gate-id [_] :refuse-gated)
    (permit? [_ spec]
      (when (ent/gated? spec) :deny/refused))))

(defn- entitlement-gate
  [licensed]
  (reify ent/ILicenseGate
    (gate-id [_] :entitlements)
    (permit? [_ spec]
      (when-not (contains? licensed (:addon/entitlement spec))
        :deny/not-entitled))))

(deftest a-plain-spec-is-ungated
  (let [manifest (opaque/->manifest plain-spec)]
    (is (ms/validate ms/MountSpec manifest)
        (str (ms/humanize-errors ms/MountSpec manifest)))
    (is (= :external (:addon/trust-class manifest)))
    (is (not (contains? ent/gated-trust-classes (:addon/trust-class manifest))))
    (is (not (contains? manifest :addon/entitlement)))
    (testing "a gate that refuses everything gated still permits it"
      (is (nil? (ent/permit refusing-gate manifest)))
      (is (nil? (ent/permit ent/closed-gate manifest))))))

(deftest a-proprietary-spec-keeps-the-gated-manifest
  (let [manifest (opaque/->manifest paid-spec)]
    (is (ms/validate ms/MountSpec manifest)
        (str (ms/humanize-errors ms/MountSpec manifest)))
    (is (= :proprietary (:addon/trust-class manifest)))
    (is (= "dev.wasm-kernel" (:addon/entitlement manifest)))
    (testing "refused when the gate denies the entitlement"
      (is (= :deny/refused (ent/permit refusing-gate manifest)))
      (is (= :deny/not-entitled (ent/permit (entitlement-gate #{}) manifest)))
      (is (= :deny/no-license-gate (ent/permit ent/closed-gate manifest))))
    (testing "and permitted when the gate grants it"
      (is (nil? (ent/permit (entitlement-gate #{"dev.wasm-kernel"}) manifest))))))

(deftest an-explicit-entitlement-wins
  (let [manifest (opaque/->manifest (assoc paid-spec :opaque/entitlement "acme:suite"))]
    (is (= "acme:suite" (:addon/entitlement manifest)))
    (is (nil? (ent/permit (entitlement-gate #{"acme:suite"}) manifest))))
  (testing "an ungated class carries an entitlement only when the spec gives one"
    (is (= "acme:suite"
           (:addon/entitlement
            (opaque/->manifest (assoc plain-spec :opaque/entitlement "acme:suite")))))))

(deftest the-spec-schema-admits-both-shapes
  (is (os/validate os/OpaqueSpec plain-spec))
  (is (os/validate os/OpaqueSpec paid-spec))
  (is (os/validate os/OpaqueSpec (assoc paid-spec :opaque/entitlement "acme:suite")))
  (testing "the vocabulary is TrustClass, not a new one"
    (is (not (os/validate os/OpaqueSpec (assoc plain-spec :opaque/trust-class :wasm))))
    (is (not (os/validate os/OpaqueSpec (assoc plain-spec :opaque/entitlement ""))))))
