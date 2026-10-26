(ns hive-addon.contract-coverage-test
  "The contract gate: every public fn under src carries an m/=> contract
   (inline, or host-side in hive-addon.contracts and hive-addon.opaque.contracts
   for the malli-free portable and opaque-kernel strata).

   The gate covers the WHOLE src tree with no exemptions. A namespace that
   cannot carry malli states its contracts from one of the two spines, which
   name every subject by its fully qualified symbol: m/=> registers under the
   namespace part of the symbol as written, so an aliased subject would
   register under the alias and never instrument (the opaque spine did exactly
   that until cov-fleet; `spine-subjects-register-under-their-own-namespace`
   pins it)."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.contracts]
            [hive-addon.opaque.contracts]
            [hive-schemas.coverage :as cov]
            [malli.core :as m]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def gated-paths ["src"])

(cov/deftest-contract-coverage every-gated-fn-carries-a-contract
  {:paths gated-paths
   :exempt {}})

(deftest the-gate-is-not-vacuous
  (let [report (cov/coverage {:paths gated-paths})]
    (is (<= 360 (count (:universe report)))
        "the universe collapsed - a gate over nothing passes for the wrong reason")
    (is (= (:universe report) (:covered report)))))

(deftest spine-subjects-register-under-their-own-namespace
  ;; Every registered function-schema namespace must be a real, loaded
  ;; namespace holding the subject var. An alias-named subject (codec/encode)
  ;; would register under 'codec, which resolves to no namespace at all.
  (let [registered (m/function-schemas)]
    (doseq [[ns-sym fns] registered
            :when (str/starts-with? (str ns-sym) "hive-addon")
            [f _] fns]
      (is (some? (find-var (symbol (str ns-sym) (str f))))
          (str ns-sym "/" f " is contracted but names no var")))
    (is (not-any? registered '[codec serve opaque t])
        "a spine contract registered under an alias")))

