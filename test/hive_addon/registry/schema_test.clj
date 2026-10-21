(ns hive-addon.registry.schema-test
  "Facets for the schema-property registry: contributions MERGE per tool, so
   several addons may widen one tool. A golden over fixed contribution scripts,
   with mutants that replace instead of merge."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-addon.contracts]
            [hive-addon.registry.schema :as rs]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each (fn [f] (rs/clear!) (f) (rs/clear!)))

(defn- script [register contributions]
  (rs/clear!)
  (let [returned (mapv (fn [[tool props]] (register tool props)) contributions)]
    {:returned returned
     :tools    (vec (sort (rs/extended-tool-names)))
     :by-tool  (into (sorted-map)
                     (map (fn [t] [t (into (sorted-map) (rs/get-extensions t))]))
                     (rs/extended-tool-names))}))

(def ^:private real-register rs/register!)

(defn script*
  "The golden subject: the real register! driven through `script`."
  [contributions]
  (script rs/register! contributions))

(tri/deftrifecta register-script
  hive-addon.registry.schema-test/script*
  {:golden-path "test/golden/hive-addon/registry-schema.edn"
   :apply?      true
   :cases       {:none   [[]]
                 :one    [[["memory" {"k" {:type "string"}}]]]
                 :merged [[["memory" {"k" {:type "string"}}]
                           ["memory" {"j" {:type "integer"}}]
                           ["kg" {"depth" {:type "integer"}}]]]}
   :mutations   [["replaces-instead-of-merging"
                  (fn [cs] (script (fn [t p]
                                     (rs/clear!)
                                     (real-register t p))
                                   cs))]
                 ["forgets-everything" (fn [_] {:returned [] :tools [] :by-tool {}})]]})

(deftest absent-tools-and-clear
  (is (nil? (rs/get-extensions "never")))
  (is (= "t" (rs/register! "t" {})))
  (is (nil? (rs/clear!)))
  (is (= [] (rs/extended-tool-names))))
