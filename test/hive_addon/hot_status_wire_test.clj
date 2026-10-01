(ns hive-addon.hot-status-wire-test
  "The hot facade's `status` is rendered by tool surfaces that serialize it, so
   every value in it must be data. Two things in this one map were not: the
   component registry hive-hot reports (it holds live callback closures, fixed
   at that source) and `no-reload`, which is a set of SYMBOLS because that is
   what hive-hot's :no-reload option wants. A symbol is no more serializable
   than a closure, so the report projects it and the option keeps the symbols."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [hive-addon.hot :as hot]))

;; SPDX-License-Identifier: MIT

(defn- wire-safe? [x]
  (or (nil? x) (boolean? x) (number? x) (string? x) (keyword? x) (coll? x)))

(defn- unsafe-leaves
  "Every leaf no serializer can be expected to render, as [class printed]."
  [form]
  (let [found (atom [])]
    (walk/postwalk (fn [x]
                     (when-not (wire-safe? x)
                       (swap! found conj [(.getName (class x)) (pr-str x)]))
                     x)
                   form)
    @found))

(deftest the-status-report-carries-no-host-objects
  (is (empty? (unsafe-leaves (hot/status)))
      "one unserializable leaf fails the whole status call, and the error names
       only that leaf's class, which points away from the report"))

(deftest no-reload-is-projected-without-losing-what-it-names
  (let [reported (:hot/no-reload (hot/status))]
    (is (every? string? reported))
    (is (= (sort (map str hot/no-reload)) reported)
        "the projection must name every pinned namespace, in a stable order")
    (testing "the option itself still holds symbols, which is what hive-hot reads"
      (is (every? symbol? hot/no-reload))
      (is (contains? hot/no-reload 'hive-addon.protocol)))))

(deftest the-report-still-answers-what-it-is-for
  (let [report (hot/status)]
    (is (contains? report :hot/available?))
    (is (vector? (:hot/strategies report)))
    (is (every? keyword? (:hot/strategies report))
        "a strategy id is a keyword, which is already wire-safe")))
