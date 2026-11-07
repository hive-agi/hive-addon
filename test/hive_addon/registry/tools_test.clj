(ns hive-addon.registry.tools-test
  "Facets for the tool registry: a golden over a fixed register/look-up
   script, with mutants of the registration semantics the script must kill.

   The registry is an atom-backed boundary, so the facet drives it through its
   own public fns inside a fixture that clears it, never by redefining it."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-addon.contracts]
            [hive-addon.registry.tools :as tools]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each (fn [f] (tools/clear!) (f) (tools/clear!)))

(defn- script
  "Run REGISTER over TOOL-DEFS on a clean registry, then answer what the
   registry shows. Data only, so the golden is plain EDN."
  [register tool-defs]
  (tools/clear!)
  (let [returned (mapv register tool-defs)]
    {:returned returned
     :names    (vec (sort (tools/registered-names)))
     :looked   (some-> (tools/get-tool (:name (first tool-defs))) (dissoc :handler))
     :count    (count (tools/registered))}))

(def ^:private real-register tools/register!)

(defn script*
  "The golden subject: the real register! driven through `script`."
  [tool-defs]
  (script tools/register! tool-defs))

(tri/deftrifecta register-script
  hive-addon.registry.tools-test/script*
  {:golden-path "test/golden/hive-addon/registry-tools.edn"
   :apply?      true
   :cases       {:one       [[{:name "a" :handler identity :description "first"}]]
                 :two       [[{:name "b" :handler inc} {:name "a" :handler dec}]]
                 :overwrite [[{:name "a" :handler inc :description "old"}
                              {:name "a" :handler dec :description "new"}]]}
   :mutations   [["drops-registration" (fn [_] {:returned [] :names [] :looked nil :count 0})]
                 ["first-write-wins"
                  (fn [defs] (script (fn [d] (if (tools/get-tool (:name d)) (:name d) (real-register d)))
                                     defs))]
                 ["returns-def-not-name"
                  (fn [defs] (assoc (script real-register defs) :returned (mapv #(dissoc % :handler) defs)))]]})

(deftest deregister-and-clear-answer-their-contracts
  (tools/register! {:name "x" :handler identity})
  (is (= "x" (tools/deregister! "x")))
  (is (nil? (tools/get-tool "x")))
  (is (= "absent" (tools/deregister! "absent")) "an unknown name is echoed, not refused")
  (tools/register! {:name "y" :handler identity})
  (is (nil? (tools/clear!)))
  (is (= [] (tools/registered))))

(deftest the-precondition-is-the-domain
  (is (thrown? AssertionError (tools/register! {:handler identity})))
  (is (thrown? AssertionError (tools/register! {:name "x"}))))
