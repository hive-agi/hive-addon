(ns hive-addon.tool-contract.test
  "clojure.test support for the root-tool contract, for any IAddon's suite.

     (ns my.addon-contract-test
       (:require [hive-addon.tool-contract.test :refer [deftest-root-tools]]
                 [hive-addon.protocol :as proto]
                 [my.addon :as addon]))

     (deftest-root-tools my-addon-root-tools
       (proto/tools (addon/->addon)))

   Each violating tool is one failed assertion naming the tool and the path.
   An empty tool seq fails unless `:allow-empty? true` is passed."
  (:require [clojure.test :as t]
            [hive-addon.tool-contract :as contract]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn assert-root-tools
  "Run the contract over `tools` as clojure.test assertions. Returns the report."
  ([tools] (assert-root-tools tools {}))
  ([tools {:keys [allow-empty?]}]
   (let [tools (vec tools)
         rep   (contract/report tools)]
     (t/is (or allow-empty? (pos? (:checked rep)))
           "no tool defs to check — pass :allow-empty? true for an addon without tools")
     (doseq [tool tools
             :let [vs (contract/violations tool)]]
       (t/is (empty? vs)
             (str "root tool contract violated by " (contract/tool-label tool) ":\n"
                  (contract/format-findings {(contract/tool-label tool) vs}))))
     rep)))

(defmacro deftest-root-tools
  "Define deftest `name` asserting that every tool def `tools-expr` evaluates
   to satisfies the root-tool contract. Options: :allow-empty? (default false)."
  [name tools-expr & {:as opts}]
  `(t/deftest ~name
     (assert-root-tools ~tools-expr ~(or opts {}))))
