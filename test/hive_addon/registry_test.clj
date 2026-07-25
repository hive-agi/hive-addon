(ns hive-addon.registry-test
  "These registries are how an addon reaches the host surface, so a
   contribution that survives shutdown, or a retraction that takes another
   addon's commands with it, corrupts the host for every other addon."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.registry.commands :as commands]
            [hive-addon.registry.extension :as ext]
            [hive-addon.registry.schema :as schema]
            [hive-addon.registry.tools :as tools]))

(use-fixtures :each (fn [f]
                      (ext/clear!) (schema/clear!) (tools/clear!) (commands/clear!)
                      (f)
                      (ext/clear!) (schema/clear!) (tools/clear!) (commands/clear!)))

;; =============================================================================
;; Opaque extensions
;; =============================================================================

(deftest register-and-look-up-test
  (is (= :cap/x (ext/register! :cap/x inc)))
  (is (= 2 ((ext/get-extension :cap/x) 1)))
  (is (true? (ext/extension-available? :cap/x)))
  (is (= #{:cap/x} (ext/registered-keys))))

(deftest an-absent-extension-yields-the-default-test
  (is (nil? (ext/get-extension :cap/missing)))
  (is (= ::fallback (ext/get-extension :cap/missing ::fallback)))
  (is (false? (ext/extension-available? :cap/missing))))

(deftest re-registration-replaces-test
  (ext/register! :cap/x inc)
  (ext/register! :cap/x dec)
  (is (= 0 ((ext/get-extension :cap/x) 1)))
  (is (= 1 (count (ext/registered-keys))) "replacing does not add a key"))

(deftest register-many-is-atomic-and-returns-its-keys-test
  (is (= #{:a :b} (set (ext/register-many! {:a inc :b dec}))))
  (is (= #{:a :b} (ext/registered-keys))))

(deftest a-non-keyword-key-or-non-invokable-value-is-rejected-test
  (is (thrown? AssertionError (ext/register! "not-a-keyword" inc)))
  (is (thrown? AssertionError (ext/register! :cap/x 42))))

(deftest deregister-removes-only-its-key-test
  (ext/register-many! {:a inc :b dec})
  (is (= :a (ext/deregister! :a)))
  (is (= #{:b} (ext/registered-keys))))

;; =============================================================================
;; Schema extensions
;; =============================================================================

(deftest schema-extensions-merge-across-addons-test
  (schema/register! "analysis" {"path" {:type "string"}})
  (schema/register! "analysis" {"depth" {:type "integer"}})
  (is (= #{"path" "depth"} (set (keys (schema/get-extensions "analysis")))))
  (is (= ["analysis"] (schema/extended-tool-names))))

(deftest schema-extensions-are-per-tool-test
  (schema/register! "a" {"x" {}})
  (schema/register! "b" {"y" {}})
  (is (= ["x"] (keys (schema/get-extensions "a"))))
  (is (nil? (schema/get-extensions "c"))))

(deftest schema-registration-validates-its-arguments-test
  (is (thrown? AssertionError (schema/register! :not-a-string {})))
  (is (thrown? AssertionError (schema/register! "tool" "not-a-map"))))

;; =============================================================================
;; Dynamic tools
;; =============================================================================

(deftest tools-register-by-name-test
  (is (= "lint" (tools/register! {:name "lint" :handler identity})))
  (is (= ["lint"] (tools/registered-names)))
  (is (= identity (:handler (tools/get-tool "lint")))))

(deftest tool-registration-is-last-write-wins-test
  (tools/register! {:name "lint" :handler identity :v 1})
  (tools/register! {:name "lint" :handler identity :v 2})
  (is (= 1 (count (tools/registered))))
  (is (= 2 (:v (tools/get-tool "lint")))))

(deftest a-tool-needs-a-name-and-a-handler-test
  (is (thrown? AssertionError (tools/register! {:handler identity})))
  (is (thrown? AssertionError (tools/register! {:name "x"}))))

;; =============================================================================
;; Composite-tool command contributions
;; =============================================================================

(deftest contributions-are-stamped-with-their-addon-test
  (commands/contribute! "analysis" :kondo {"lint" {:handler identity}})
  (is (= :kondo (get-in (commands/get-commands "analysis") ["lint" :addon]))))

(deftest keyword-command-names-are-coerced-to-strings-test
  (commands/contribute! "analysis" :kondo {:lint {:handler identity}})
  (is (contains? (commands/get-commands "analysis") "lint")))

(deftest retract-removes-only-the-named-addons-commands-test
  (commands/contribute! "analysis" :kondo {"lint" {:handler identity}})
  (commands/contribute! "analysis" :scc {"count" {:handler identity}})
  (commands/retract! "analysis" :kondo)
  (is (= ["count"] (keys (commands/get-commands "analysis")))
      "the other addon's contribution must survive"))

(deftest retract-all-sweeps-every-tool-test
  (commands/contribute! "a" :kondo {"x" {:handler identity}})
  (commands/contribute! "b" :kondo {"y" {:handler identity}})
  (commands/contribute! "b" :scc {"z" {:handler identity}})
  (commands/retract-all! :kondo)
  (is (empty? (commands/get-commands "a")))
  (is (= ["z"] (keys (commands/get-commands "b")))))

(deftest retracting-an-unknown-addon-is-a-no-op-test
  (commands/contribute! "a" :kondo {"x" {:handler identity}})
  (commands/retract-all! :never-registered)
  (is (= ["x"] (keys (commands/get-commands "a")))))

(deftest contributed-tool-names-lists-every-host-test
  (commands/contribute! "a" :k {"x" {:handler identity}})
  (commands/contribute! "b" :k {"y" {:handler identity}})
  (is (= #{"a" "b"} (set (commands/contributed-tool-names)))))

;; =============================================================================
;; Isolation between registries
;; =============================================================================

(deftest each-registry-clears-independently-test
  (testing "clearing one registry must not empty the others"
    (ext/register! :a inc)
    (schema/register! "t" {"p" {}})
    (tools/register! {:name "t" :handler identity})
    (commands/contribute! "t" :a {"c" {:handler identity}})
    (ext/clear!)
    (is (empty? (ext/registered-keys)))
    (is (some? (schema/get-extensions "t")))
    (is (= 1 (count (tools/registered))))
    (is (some? (commands/get-commands "t")))))
