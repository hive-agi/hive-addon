(ns hive-addon.terminal-vessel-test
  "ITerminalAddon and IVessel: a reify implementing every method dispatches,
   and the predicates tell addon-only, terminal-only, vessel-only and combined
   objects apart."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.protocol :as addon]
            [hive-addon.terminal :as term]
            [hive-addon.vessel :as vessel]))

(defn- iaddon []
  (reify addon/IAddon
    (addon-id          [_] "demo.terminal")
    (addon-type        [_] :native)
    (capabilities      [_] #{:tools})
    (initialize!       [_ _] {:success? true})
    (shutdown!         [_] nil)
    (tools             [_] [])
    (schema-extensions [_] [])
    (health            [_] {:status :ok})
    (excluded-tools    [_] #{})
    (hooks             [_] {})))

(defn- terminal-addon []
  (reify
    addon/IAddon
    (addon-id          [_] "demo.terminal")
    (addon-type        [_] :native)
    (capabilities      [_] #{:tools})
    (initialize!       [_ _] {:success? true})
    (shutdown!         [_] nil)
    (tools             [_] [])
    (schema-extensions [_] [])
    (health            [_] {:status :ok})
    (excluded-tools    [_] #{})
    (hooks             [_] {})
    term/ITerminalAddon
    (terminal-id         [_] :demo)
    (terminal-spawn!     [_ ctx _] (str "slave-" (:id ctx)))
    (terminal-dispatch!  [_ _ _] true)
    (terminal-status     [_ ctx _] {:slave/id (:id ctx) :slave/status :idle})
    (terminal-kill!      [_ ctx] {:killed? true :id (:id ctx)})
    (terminal-interrupt! [_ ctx] {:success? true :ling-id (:id ctx)})))

(defn- terminal-only []
  (reify term/ITerminalAddon
    (terminal-id         [_] :bare)
    (terminal-spawn!     [_ _ _] "s")
    (terminal-dispatch!  [_ _ _] true)
    (terminal-status     [_ _ _] nil)
    (terminal-kill!      [_ ctx] {:killed? false :id (:id ctx) :reason :absent})
    (terminal-interrupt! [_ ctx] {:success? false :ling-id (:id ctx) :errors ["no"]})))

(defn- demo-vessel [term-impl]
  (reify vessel/IVessel
    (vessel-id       [_] :demo-vessel)
    (capabilities    [_] #{:terminal})
    (resolve-context [_ agent-id]
      (when (= agent-id "ling-1")
        {:project-id "p" :cwd "/tmp" :session-id "s1"}))
    (addon           [_ cap] (when (= cap :terminal) term-impl))
    (initialize!     [_ config] {:initialized config})
    (shutdown!       [_] :stopped)))

(deftest terminal-dispatches-every-method
  (let [t   (terminal-addon)
        ctx {:id "ling-1"}]
    (is (= :demo (term/terminal-id t)))
    (is (= "slave-ling-1" (term/terminal-spawn! t ctx {:task "x"})))
    (is (true? (term/terminal-dispatch! t ctx {:task "x"})))
    (is (= {:slave/id "ling-1" :slave/status :idle}
           (term/terminal-status t ctx nil)))
    (is (= {:killed? true :id "ling-1"} (term/terminal-kill! t ctx)))
    (is (= {:success? true :ling-id "ling-1"} (term/terminal-interrupt! t ctx)))))

(deftest terminal-addon-predicate
  (testing "true only when both protocols are satisfied"
    (is (true? (term/terminal-addon? (terminal-addon))))
    (is (false? (term/terminal-addon? (iaddon))))
    (is (false? (term/terminal-addon? (terminal-only))))
    (is (false? (term/terminal-addon? {})))
    (is (false? (term/terminal-addon? nil)))))

(deftest terminal-failure-shapes
  (let [t (terminal-only)]
    (is (nil? (term/terminal-status t {:id "x"} {:slave/status :dead})))
    (is (= :absent (:reason (term/terminal-kill! t {:id "x"}))))
    (is (= ["no"] (:errors (term/terminal-interrupt! t {:id "x"}))))))

(deftest vessel-dispatches-every-method
  (let [t (terminal-addon)
        v (demo-vessel t)]
    (is (= :demo-vessel (vessel/vessel-id v)))
    (is (= #{:terminal} (vessel/capabilities v)))
    (is (= {:project-id "p" :cwd "/tmp" :session-id "s1"}
           (vessel/resolve-context v "ling-1")))
    (is (nil? (vessel/resolve-context v "unknown")))
    (is (identical? t (vessel/addon v :terminal)))
    (is (nil? (vessel/addon v :editor)))
    (is (= {:initialized {:a 1}} (vessel/initialize! v {:a 1})))
    (is (= :stopped (vessel/shutdown! v)))))

(deftest vessel-predicates
  (let [v (demo-vessel nil)]
    (is (true? (vessel/vessel? v)))
    (is (false? (vessel/vessel? (iaddon))))
    (is (false? (vessel/vessel? nil)))
    (testing "a vessel and its addon are normally separate objects"
      (is (false? (vessel/vessel-addon? v)))
      (is (false? (vessel/vessel-addon? (iaddon)))))))

(deftest reload-guard-keeps-protocol
  (testing "the guarded defprotocol is defined exactly once"
    (is (some? vessel/IVessel))
    (is (contains? (set (keys (:sigs vessel/IVessel))) :resolve-context))))
