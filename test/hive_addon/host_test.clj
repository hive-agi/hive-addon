(ns hive-addon.host-test
  "A host seam that throws on an absent host reintroduces exactly the load
   failure it exists to prevent; one that caches absence, or captures the
   host's VALUE instead of its var, goes permanently stale the moment the host
   loads or reloads."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.host :as host]
            [hive-dsl.result :as r]))

;; =============================================================================
;; Fixture host: a namespace that does not exist on disk, interned at runtime
;; so "the host arrives later" and "the host is reloaded" are both expressible.
;; =============================================================================

(def ^:private fixture-ns 'hive-addon.host-test.fake-host)

(defn- install-host!
  "Intern `f` as fixture-ns/greet, creating the namespace on first call.
   Re-interning rebinds the SAME var, which is what a namespace reload does."
  [f]
  (intern (create-ns fixture-ns) 'greet f))

(defn- uninstall-host! []
  (remove-ns fixture-ns)
  nil)

(def ^:private greet-sym (symbol (name fixture-ns) "greet"))

;; =============================================================================
;; resolve-var / available?
;; =============================================================================

(deftest resolve-var-never-throws-test
  (testing "an absent namespace resolves to nil rather than throwing"
    (is (nil? (host/resolve-var 'totally.absent.ns/nope))))
  (testing "a present namespace missing the name resolves to nil"
    (is (nil? (host/resolve-var 'clojure.core/no-such-var-here))))
  (testing "a present var resolves"
    (is (= #'clojure.core/inc (host/resolve-var 'clojure.core/inc)))))

(deftest available?-test
  (is (true? (host/available? 'clojure.core/inc)))
  (is (false? (host/available? 'totally.absent.ns/nope))))

;; =============================================================================
;; soft: absence is a Result, not a throw
;; =============================================================================

(deftest soft-absent-returns-err-test
  (let [f (host/soft 'totally.absent.ns/nope)
        res (f 1 2 3)]
    (is (r/err? res))
    (is (= :host/absent (:error res)))
    (is (= "totally.absent.ns/nope" (:host/sym res)))))

(deftest soft-absent-fn-receives-the-same-args-test
  (let [f (host/soft 'totally.absent.ns/nope (fn [& args] {:degraded (vec args)}))]
    (is (= {:degraded [1 2]} (f 1 2)))
    (is (= {:degraded []} (f)))))

(deftest soft-invokes-the-host-when-present-test
  (let [f (host/soft 'clojure.core/+)]
    (is (= 6 (f 1 2 3)))))

;; =============================================================================
;; No negative cache: the host may load after the addon
;; =============================================================================

(deftest soft-picks-up-a-host-that-arrives-later-test
  (uninstall-host!)
  (try
    (let [f (host/soft greet-sym)]
      (is (r/err? (f "a")) "absent before the host exists")
      (install-host! (fn [x] (str "hello " x)))
      (is (= "hello a" (f "a")) "the same soft fn sees the host once it loads"))
    (finally (uninstall-host!))))

;; =============================================================================
;; Capture-by-Var: a positive cache holds the VAR, not the value
;; =============================================================================

(deftest soft-resolves-through-the-var-across-a-reload-test
  (uninstall-host!)
  (try
    (install-host! (fn [x] (str "v1 " x)))
    (let [f (host/soft greet-sym)]
      (is (= "v1 a" (f "a")) "first call populates the positive cache")
      (install-host! (fn [x] (str "v2 " x)))
      (is (= "v2 a" (f "a")) "a rebound host var is seen by the next call"))
    (finally (uninstall-host!))))

;; =============================================================================
;; defsoft / api
;; =============================================================================

(host/defsoft plus 'clojure.core/+ :doc "Soft +")
(host/defsoft missing 'totally.absent.ns/nope)
(host/defsoft missing-degraded 'totally.absent.ns/nope :absent (constantly :degraded))

(deftest defsoft-test
  (is (= 3 (plus 1 2)))
  (is (= "Soft +" (:doc (meta #'plus))))
  (is (r/err? (missing)))
  (is (= :degraded (missing-degraded 1 2)))
  (testing "the default docstring names the host symbol"
    (is (re-find #"totally\.absent\.ns/nope" (:doc (meta #'missing))))))

(deftest api-builds-a-var-map-test
  (let [m (host/api {:plus 'clojure.core/+
                     :gone 'totally.absent.ns/nope
                     :gone-degraded ['totally.absent.ns/nope (constantly :fallback)]})]
    (is (= #{:plus :gone :gone-degraded} (set (keys m))))
    (is (= 3 ((:plus m) 1 2)))
    (is (r/err? ((:gone m))))
    (is (= :fallback ((:gone-degraded m) 1)))))
