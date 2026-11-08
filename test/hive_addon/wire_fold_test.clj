(ns hive-addon.wire-fold-test
  "Generative facets for json-safe, beside the example tests in
   hive-addon.wire-test: the fold always lands in the WireValue its contract
   promises and is idempotent, and a golden over host values with mutants that
   leak a live object or lose determinism."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.contracts :as c]
            [hive-addon.wire :as wire]
            [hive-test.trifecta :as tri]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private gen-host-value
  "Values with live objects mixed in: fns, symbols, sets, odd map keys."
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of [(gen/vector inner 0 3)
                  (gen/set inner {:max-elements 3})
                  (gen/map (gen/one-of [gen/keyword gen/string-alphanumeric gen/small-integer gen/symbol])
                           inner {:max-elements 3})]))
   (gen/one-of [gen/small-integer gen/string-alphanumeric gen/keyword gen/symbol gen/boolean
                (gen/return nil) (gen/return inc) (gen/return (Object.))])))

(defspec json-safe-lands-in-the-wire-vocabulary 200
  (prop/for-all [v gen-host-value]
    (m/validate c/WireValue (wire/json-safe v))))

(defspec json-safe-is-idempotent 200
  (prop/for-all [v gen-host-value]
    (let [once (wire/json-safe v)] (= once (wire/json-safe once)))))

(tri/deftrifecta fold
  hive-addon.wire/json-safe
  {:golden-path "test/golden/hive-addon/wire-json-safe.edn"
   :cases       {:scalar 42
                 :symbol 'a/b
                 :fn     inc
                 :set    #{:b :a "c"}
                 :keys   {1 :one 'sym [inc] "s" #{2 1}}}
   :mutations   [["fn-identity-leaks" (fn [x] (if (fn? x) (str x) (wire/json-safe x)))]
                 ["sets-unsorted" (fn [x] (if (set? x) (mapv wire/json-safe (reverse (sort-by str x))) (wire/json-safe x)))]
                 ["identity" identity]]})

(deftest a-live-object-never-reaches-the-wire
  (is (string? (wire/json-safe (Object.)))))
