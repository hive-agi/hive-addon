(ns hive-addon.protocol-test
  "IAddon contract: a record implementing every method satisfies the protocol,
   dispatches correctly, and the predicate/constant helpers hold."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.protocol :as addon]))

(defrecord DemoAddon [id]
  addon/IAddon
  (addon-id          [_] id)
  (addon-type        [_] :native)
  (capabilities      [_] #{:tools :health-reporting})
  (initialize!       [_ _config] {:success? true})
  (shutdown!         [_] nil)
  (tools             [_] [{:name "demo_tool"}])
  (schema-extensions [_] [])
  (health            [_] {:status :ok})
  (excluded-tools    [_] #{})
  (hooks             [_] {}))

(deftest satisfies-and-dispatch
  (let [a (->DemoAddon "demo.addon")]
    (is (addon/addon? a))
    (is (satisfies? addon/IAddon a))
    (is (= "demo.addon" (addon/addon-id a)))
    (is (= :native (addon/addon-type a)))
    (is (= #{:tools :health-reporting} (addon/capabilities a)))
    (is (:success? (addon/initialize! a {})))
    (is (nil? (addon/shutdown! a)))
    (is (= [{:name "demo_tool"}] (addon/tools a)))
    (is (= #{} (addon/excluded-tools a)))
    (is (= {} (addon/hooks a)))))

(deftest type-predicates
  (is (addon/valid-addon-type? :native))
  (is (addon/valid-addon-type? :mcp-bridge))
  (is (addon/valid-addon-type? :external))
  (is (not (addon/valid-addon-type? :bogus)))
  (is (= 3 (count addon/valid-addon-types))))

(deftest health-predicates
  (testing "status helpers over a health map"
    (is (addon/healthy?  {:status :ok}))
    (is (addon/degraded? {:status :degraded}))
    (is (addon/down?     {:status :down}))
    (is (not (addon/healthy? {:status :down})))
    (is (contains? addon/health-statuses :ok))))

(deftest standard-capabilities-present
  (is (contains? addon/standard-capabilities :tools))
  (is (contains? addon/standard-capabilities :resources)))
