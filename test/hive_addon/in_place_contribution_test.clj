(ns hive-addon.in-place-contribution-test
  "An addon on the :in-place strategy contributes a verb to a host tool. After
   its namespace is unloaded and recreated (what clj-reload does), the verb the
   registry dispatches to must be the new code, not a closure over the old vars."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.hot.strategy :as strategy]
            [hive-addon.registry.commands :as cmds]))

(def addon-ns 'hive-addon.test-contrib-addon)
(def contribution-sym 'hive-addon.test-contrib-addon/contribution)

(defn- define-addon-ns!
  "Unload ADDON-NS if present, then define it afresh so its verb answers
   GENERATION: the shape of a clj-reload unload + load."
  [generation]
  (remove-ns addon-ns)
  (binding [*ns* *ns*]
    (eval `(ns ~addon-ns))
    (eval `(defn ~'wait-handler [~'_params] {:generation ~generation}))
    (eval `(def ~'contribution {"wait" {:handler ~'wait-handler
                                        :description "test verb"}}))))

(defn- resolve-contribution [sym]
  (some-> (find-var sym) deref))

(defn- call-verb []
  ((get-in (cmds/get-commands "probe-tool") ["wait" :handler]) {}))

(use-fixtures :each
  (fn [t]
    (cmds/clear!)
    (try (t) (finally (cmds/clear!) (remove-ns addon-ns)))))

(def spec {:addon/id "test.contrib"
           :addon/init-ns (str addon-ns)
           :addon/init-fn "make"
           :addon/reload-strategy :in-place})

(defn- in-place-reload! [generation]
  (strategy/reload! spec {:hot/reload-ns! (fn [nss]
                                            (define-addon-ns! generation)
                                            {:loaded (vec nss)})
                          :hot/resolve-contribution resolve-contribution}))

(deftest in-place-reload-makes-the-new-verb-live
  (define-addon-ns! 1)
  (cmds/contribute-from! "probe-tool" :test.contrib.verbs contribution-sym resolve-contribution)
  (is (= {:generation 1} (call-verb)))
  (let [report (in-place-reload! 2)]
    (is (:ok? report))
    (is (= :in-place (:hot/strategy report)))
    (is (= {"probe-tool" ["wait"]} (:hot/contributions-refreshed report))))
  (testing "the registry dispatches to the reloaded code"
    (is (= {:generation 2} (call-verb))))
  (testing "the contribution keeps its original addon attribution"
    (is (= :test.contrib.verbs (get-in (cmds/get-commands "probe-tool") ["wait" :addon])))))

(deftest refresh-only-touches-reloaded-namespaces
  (define-addon-ns! 1)
  (cmds/contribute-from! "probe-tool" :test.contrib.verbs contribution-sym resolve-contribution)
  (define-addon-ns! 2)
  (is (= {} (cmds/refresh-namespaces! ["some.other.ns"] resolve-contribution)))
  (is (= {:generation 1} (call-verb)) "an unrelated reload leaves the verb as it was")
  (is (= {"probe-tool" ["wait"]}
         (cmds/refresh-namespaces! [(str addon-ns)] resolve-contribution)))
  (is (= {:generation 2} (call-verb))))

(deftest retracted-contributions-are-not-resurrected
  (define-addon-ns! 1)
  (cmds/contribute-from! "probe-tool" :test.contrib.verbs contribution-sym resolve-contribution)
  (cmds/retract-all! :test.contrib.verbs)
  (define-addon-ns! 2)
  (is (= {} (cmds/refresh-namespaces! [(str addon-ns)] resolve-contribution)))
  (is (empty? (cmds/get-commands "probe-tool"))))

(deftest plain-contribute-is-unchanged
  (define-addon-ns! 1)
  (cmds/contribute! "probe-tool" :plain @(find-var contribution-sym))
  (define-addon-ns! 2)
  (is (= {} (cmds/refresh-namespaces! [(str addon-ns)] resolve-contribution))
      "a value contribution names no source, so there is nothing to re-resolve"))
