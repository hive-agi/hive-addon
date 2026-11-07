(ns hive-addon.registry.commands-test
  "Facets for command contributions: a golden over a fixed contribute/retract
   script that records both the store and the listener events, with mutants of
   the retraction semantics (the part that, wrong, takes another addon's
   commands with it)."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-addon.contracts]
            [hive-addon.registry.commands :as cmds]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- reset-all! []
  (cmds/clear!)
  (doseq [id (cmds/listener-ids)] (cmds/remove-listener! id)))

(use-fixtures :each (fn [f] (reset-all!) (f) (reset-all!)))

(defn- view
  "The store as plain data: tool -> command -> contributing addon."
  []
  (into (sorted-map)
        (map (fn [t] [t (into (sorted-map) (map (fn [[c spec]] [c (:addon spec)])) (cmds/get-commands t))]))
        (cmds/contributed-tool-names)))

(defn- run-script
  "Apply STEPS ([:contribute tool addon cmds] | [:retract tool addon] |
   [:retract-all addon]) with RETRACT-ALL as the retract-all implementation,
   recording every listener event."
  [retract-all steps]
  (reset-all!)
  (let [events (atom [])]
    (cmds/add-listener! ::rec #(swap! events conj (select-keys % [:type :tool-name :addon-id])))
    (doseq [[op & args] steps]
      (case op
        :contribute  (apply cmds/contribute! args)
        :retract     (apply cmds/retract! args)
        :retract-all (apply retract-all args)))
    {:store (view) :events @events}))

(def ^:private h (constantly :ok))

(def ^:private two-addons
  [[:contribute "analysis" "lint" {:check {:handler h} :fix {:handler h}}]
   [:contribute "analysis" "carto" {:graph {:handler h}}]
   [:contribute "memory" "carto" {"index" {:handler h}}]])

(defn script*
  "The golden subject: the real retract-all! driven through `run-script`."
  [steps]
  (run-script cmds/retract-all! steps))

(tri/deftrifecta contribute-retract-script
  hive-addon.registry.commands-test/script*
  {:golden-path "test/golden/hive-addon/registry-commands.edn"
   :apply?      true
   :cases       {:contribute-only [two-addons]
                 :retract-one     [(conj two-addons [:retract "analysis" "lint"])]
                 :retract-all     [(conj two-addons [:retract-all "carto"])]}
   :mutations   [["retract-all-is-a-no-op" (fn [steps] (run-script (fn [_] nil) steps))]
                 ["retract-all-clears-everyone"
                  (fn [steps] (run-script (fn [_] (cmds/clear!)) steps))]
                 ["retract-all-skips-events"
                  (fn [steps] (-> (run-script cmds/retract-all! steps)
                                  (update :events (fn [es] (filterv #(not= :retract (:type %)) es)))))]]})

(deftest listener-ids-are-idempotent-by-id
  (is (= ::a (cmds/add-listener! ::a identity)))
  (is (= ::a (cmds/add-listener! ::a identity)))
  (is (= [::a] (cmds/listener-ids)))
  (is (= ::a (cmds/remove-listener! ::a)))
  (is (= ::absent (cmds/remove-listener! ::absent)) "removing an absent id is safe"))

(deftest contribute-answers-the-stringified-names
  (is (= #{"x" "y"} (set (cmds/contribute! "t" "a" {:x {:handler h} "y" {:handler h}}))))
  (is (nil? (cmds/retract! "t" "a")))
  (is (= {} (cmds/get-commands "t"))))
