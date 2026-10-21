(ns hive-addon.hot.strategy-test
  "Facets for the reload strategy chain: which built-in strategy `select`
   picks for each spec shape, and `strategy-by-id` over the fresh chain. The
   context is the reload context's source reading, so each case is a spec
   whose code lives in a directory, a jar, or nowhere."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-addon.hot.strategy :as strategy]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each (fn [t] (strategy/reset-strategies!) (t) (strategy/reset-strategies!)))

(defn selected
  "The id select answers (or its error key) for a spec whose source is KIND,
   optionally declaring a strategy."
  [kind declared]
  (let [spec (cond-> {:addon/id "probe" :addon/init-ns "probe.init"}
               declared (assoc :addon/reload-strategy declared))
        ctx  {:hot/source {:hot/source-kind kind :hot/reloadable? (= :directory kind)}}
        sel  (strategy/select spec ctx)]
    (if (:ok sel) (strategy/-strategy-id (:ok sel)) (:error sel))))

(tri/deftrifecta selection-by-source
  hive-addon.hot.strategy-test/selected
  {:golden-path "test/golden/hive-addon/strategy-select.edn"
   :apply? true
   :cases {:directory [:directory nil]
           :jar [:jar nil]
           :absent [:absent nil]
           :declared-inert [:jar :inert]
           :declared-unknown [:directory :no-such-strategy]}
   :mutations [["always-remount" (fn [_ _] :remount)]
               ["unknown-falls-back" (fn [k d] (let [s (selected k d)]
                                                 (if (= :hot/unknown-strategy s) (selected k nil) s)))]]})

(deftest strategy-by-id-finds-only-chain-members
  (let [chain (strategy/default-strategies)]
    (is (every? #(= % (strategy/-strategy-id (strategy/strategy-by-id chain %)))
                (map strategy/-strategy-id chain)))
    (is (nil? (strategy/strategy-by-id chain :no-such-strategy)))))
