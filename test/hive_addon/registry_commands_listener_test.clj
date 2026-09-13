(ns hive-addon.registry-commands-listener-test
  "The store alone was never the whole concept. A host builds its advertised tool
   surface FROM these contributions, so a contribution arriving after that
   surface exists has to say so. While listeners lived only on the host side, an
   addon that migrated to this registry still worked at boot (the surface is
   assembled afterwards anyway) and silently lost any post-boot contribution:
   correct on the path everyone tests, wrong on the path nobody does.

   These tests pin the seam that makes migrating an addon safe."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.registry.commands :as cmds]))

;; SPDX-License-Identifier: MIT

(def ^:private tool "analysis")

(defn- reset-world! []
  (doseq [id (cmds/listener-ids)] (cmds/remove-listener! id))
  (cmds/clear!))

(use-fixtures :each (fn [t] (reset-world!) (t) (reset-world!)))

(defn- recording-listener []
  (let [seen (atom [])]
    [seen (fn [e] (swap! seen conj e))]))

(deftest a-contribution-reaches-the-listener
  (let [[seen f] (recording-listener)]
    (cmds/add-listener! ::probe f)
    (cmds/contribute! tool :kondo {"lint" {:handler identity}})
    (is (= 1 (count @seen)))
    (let [e (first @seen)]
      (is (= :contribute (:type e)))
      (is (= tool (:tool-name e)))
      (is (= :kondo (:addon-id e)))
      (is (= ["lint"] (:commands e))
          "the surface needs the NAMES to advertise, not just the fact"))))

(deftest a-retraction-reaches-the-listener
  (let [[seen f] (recording-listener)]
    (cmds/contribute! tool :kondo {"lint" {:handler identity}})
    (cmds/add-listener! ::probe f)
    (cmds/retract! tool :kondo)
    (is (= [:retract] (mapv :type @seen)))
    (is (= tool (:tool-name (first @seen))))))

(deftest retract-all-notifies-once-per-tool-actually-touched
  (let [[seen f] (recording-listener)]
    (cmds/contribute! "analysis" :kondo {"lint" {:handler identity}})
    (cmds/contribute! "code" :kondo {"fmt" {:handler identity}})
    (cmds/contribute! "memory" :other {"add" {:handler identity}})
    (cmds/add-listener! ::probe f)
    (cmds/retract-all! :kondo)
    (is (= #{"analysis" "code"} (set (map :tool-name @seen)))
        "a tool this addon never touched must not be announced as changed")
    (is (= 2 (count @seen)))))

(deftest listeners-are-idempotent-by-id-and-removable
  (let [[a fa] (recording-listener)
        [b fb] (recording-listener)]
    (cmds/add-listener! ::probe fa)
    (cmds/add-listener! ::probe fb)
    (cmds/contribute! tool :kondo {"lint" {:handler identity}})
    (is (empty? @a) "re-registering an id replaces, it does not stack")
    (is (= 1 (count @b)))
    (cmds/remove-listener! ::probe)
    (cmds/contribute! tool :kondo {"other" {:handler identity}})
    (is (= 1 (count @b)) "a removed listener stops receiving")
    (testing "removing an absent id is safe"
      (is (= ::never (cmds/remove-listener! ::never))))))

(deftest a-listener-must-guard-itself-and-that-is-the-documented-contract
  (testing "this ns is in the portable three-host stratum, which carries zero
            reader conditionals, so it cannot catch a platform exception class
            on a listener's behalf. The contract is stated in add-listener! and
            pinned here: an unguarded thrower propagates."
    (cmds/add-listener! ::boom (fn [_] (throw (ex-info "boom" {}))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (cmds/contribute! tool :kondo {"lint" {:handler identity}}))))
  (testing "a listener that wraps itself costs nobody else their event, which is
            what a JVM host should register"
    (reset-world!)
    (let [[seen f] (recording-listener)]
      (cmds/add-listener! ::boom (fn [_] (try (throw (ex-info "boom" {}))
                                              (catch Throwable _ nil))))
      (cmds/add-listener! ::probe f)
      (is (= ["lint"] (cmds/contribute! tool :kondo {"lint" {:handler identity}})))
      (is (= ["lint"] (keys (cmds/get-commands tool))))
      (is (= 1 (count @seen))))))

(deftest the-store-is-unchanged-when-nobody-listens
  (testing "the seam is additive: with no listeners the registry behaves exactly
            as it did before it had any"
    (is (= ["lint"] (cmds/contribute! tool :kondo {"lint" {:handler identity}})))
    (is (= :kondo (:addon (get (cmds/get-commands tool) "lint"))))
    (is (= [tool] (cmds/contributed-tool-names)))
    (is (nil? (cmds/retract! tool :kondo)))
    (is (empty? (cmds/get-commands tool)))))
