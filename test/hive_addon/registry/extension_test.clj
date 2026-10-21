(ns hive-addon.registry.extension-test
  "Facets for the opaque capability registry: a golden over a fixed
   register/look-up script, with mutants of the registration semantics."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-addon.contracts]
            [hive-addon.registry.extension :as ext]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each (fn [f] (ext/clear!) (f) (ext/clear!)))

(defn- probe
  "What the registry answers about K after a script: present?, the value of
   calling the registered fn on 10, and the default-bearing lookup."
  [k]
  {:available? (ext/extension-available? k)
   :at-10      (some-> (ext/get-extension k) (apply [10]))
   :or-default (if (ext/extension-available? k) :present (ext/get-extension k :absent))})

(defn- script [register-many entries]
  (ext/clear!)
  (let [returned (register-many entries)]
    {:returned (vec (sort (or returned [])))
     :keys     (vec (sort (ext/registered-keys)))
     :probes   (into (sorted-map) (map (fn [k] [k (probe k)])) [:cap/inc :cap/dec :cap/missing])}))

(def ^:private real-register-many ext/register-many!)

(defn script*
  "The golden subject: the real register-many! driven through `script`."
  [entries]
  (script ext/register-many! entries))

(tri/deftrifecta register-many-script
  hive-addon.registry.extension-test/script*
  {:golden-path "test/golden/hive-addon/registry-extension.edn"
   :apply?      true
   :cases       {:empty [{}]
                 :one   [{:cap/inc inc}]
                 :two   [{:cap/inc inc :cap/dec dec}]}
   :mutations   [["registers-nothing" (fn [_] (script (fn [_] nil) {}))]
                 ["drops-last-entry"
                  (fn [m] (script real-register-many (into {} (butlast (sort-by key m)))))]
                 ["swaps-fns"
                  (fn [m] (script real-register-many
                                  (into {} (map (fn [[k _]] [k identity])) m)))]]})

(deftest single-register-and-deregister-answer-their-contracts
  (is (= :cap/x (ext/register! :cap/x inc)))
  (is (= :cap/x (ext/deregister! :cap/x)))
  (is (false? (ext/extension-available? :cap/x)))
  (is (nil? (ext/clear!))))
