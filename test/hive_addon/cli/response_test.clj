(ns hive-addon.cli.response-test
  "Facets for the CLI result builders: text/error as a golden pair with
   mutants that confuse the two, and error? as a predicate over both."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.cli.response :as response]
            [hive-addon.contracts]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn both
  "Both builders over one message, and error? read back from each."
  [msg]
  (let [t (response/text msg) e (response/error msg)]
    {:text t :error e :text-error? (response/error? t) :error-error? (response/error? e)}))

(tri/deftrifecta builders
  hive-addon.cli.response-test/both
  {:golden-path "test/golden/hive-addon/cli-response.edn"
   :cases       {:plain "done" :empty "" :multi "a\nb"}
   :mutations   [["error-not-flagged" (fn [m] (assoc (both m) :error {:type "text" :text m} :error-error? false))]
                 ["text-flagged"      (fn [m] (assoc (both m) :text-error? true))]
                 ["drops-message"     (fn [_] (both nil))]]})

(deftest error?-is-strict-about-true
  (is (false? (response/error? {:isError "yes"})))
  (is (false? (response/error? nil))))
