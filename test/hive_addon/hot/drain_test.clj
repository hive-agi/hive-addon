(ns hive-addon.hot.drain-test
  "The drain gate a plug OUT waits on, and eject! refusing to shut an addon
   down under a running call.

   The gate is reached through IDrainGate: each test builds its own
   atom-drain-gate and hands it to eject! as :drain-gate, and time is a stub
   clock (:now / :sleep!), so no test sleeps for the bound it asserts."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.hot-fixture :as fx]
            [hive-addon.hot.drain :as drain]
            [hive-addon.hot.inject :as inject]
            [hive-addon.hot.port :as hport]
            [hive-addon.hot.schema :as hs]
            [hive-addon.mount :as mount]
            [hive-addon.mount.injected :as injected]
            [hive-addon.mount.port :as port]
            [hive-dsl.result :as r]
            [hive-test.trifecta :as tri]
            [hive-addon.lifecycle]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each
  (fn [f]
    (fx/reset-fixture!)
    (injected/reset-registry!)
    (try (f) (finally (injected/reset-registry!)))))

(def spec-a {:addon/id "probe.a" :addon/type :native
             :addon/init-ns "hive-addon.hot-fixture" :addon/init-fn "make-a"})

(defn- mounted-host [] (doto (mount/atom-mount-host) (->> (mount/mount! (mount/solve [spec-a])))))

(defn- stub-clock
  "A clock that advances only when slept: {:now :sleep!} for drain!."
  []
  (let [t (atom 0)]
    {:now #(deref t) :sleep! #(swap! t + %)}))

(def ^:private no-dirs
  (reify hport/IHotDirs
    (-extend-dirs! [_ req] (r/ok {:dirs (:dirs req) :added []}))
    (-remove-dirs! [_ _] (r/ok {:removed [] :kept [] :absent [] :dirs [] :shared {}}))))

;; =============================================================================
;; drain-verdict: the pure reading of a DrainReport
;; =============================================================================

(tri/deftrifecta drain-verdict-reading
  hive-addon.hot.drain/drain-verdict
  {:golden-path "test/golden/hive-addon/drain-verdict.edn"
   :apply? true
   :cases {:drained         [{:hot/in-flight {}} false]
           :drained-forced  [{:hot/in-flight {}} true]
           :busy            [{:hot/in-flight {"a" 1}} false]
           :busy-forced     [{:hot/in-flight {"a" 2 "b" 1}} true]
           :no-key          [{} false]}
   :mutations
   [;; Shuts the addon down under a running call: the defect this gate exists for.
    ["always-drained" (fn [_ _] :drained)]
    ;; Ignores :force?, so a stuck call can never be overridden.
    ["force-ignored" (fn [rep _] (if (empty? (:hot/in-flight rep)) :drained :busy))]
    ;; Reports force even when nothing was running.
    ["force-wins" (fn [rep force?] (cond force? :forced
                                         (empty? (:hot/in-flight rep)) :drained
                                         :else :busy))]]})

;; =============================================================================
;; The gate and the dispatch wrap
;; =============================================================================

(deftest a-tracked-call-is-counted-while-it-runs
  (let [gate (drain/atom-drain-gate)
        seen (atom nil)
        h    (drain/tracked gate "a" (fn [x] (reset! seen (hport/-in-flight gate "a")) (inc x)))]
    (is (= 2 (h 1)))
    (is (= 1 @seen) "counted during the call")
    (is (zero? (hport/-in-flight gate "a")) "released after it")
    (testing "a call that throws is released too"
      (let [boom (drain/tracked gate "a" (fn [] (throw (ex-info "x" {}))))]
        (is (thrown? clojure.lang.ExceptionInfo (boom)))
        (is (zero? (hport/-in-flight gate "a")))))))

(deftest a-closed-gate-refuses-new-calls-and-reopens
  (let [gate (drain/atom-drain-gate)
        ran  (atom 0)
        h    (drain/tracked gate "a" (fn [] (swap! ran inc)))]
    (hport/-close! gate "a")
    (let [ex (try (h) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (= :hot/draining (:error (ex-data ex))))
      (is (= "a" (:addon/id (ex-data ex)))))
    (is (zero? @ran) "the refused call never ran")
    (testing "another addon's gate is untouched"
      (is (= 1 ((drain/tracked gate "b" (constantly 1))))))
    (drain/open! gate ["a"])
    (h)
    (is (= 1 @ran))))

(deftest dispatch-handler-gives-any-addon-the-gate-for-free
  (let [gate  (drain/atom-drain-gate)
        order (atom [])
        inner (fn [id h] (fn [& a] (swap! order conj [:inner id (hport/-in-flight gate id)]) (apply h a)))
        h     (drain/dispatch-handler "a" (fn [x] (swap! order conj :handler) x)
                                      {:inner inner :gate gate
                                       :refused (fn [msg id] {:refused id :msg msg})})]
    (testing "the host's inner wrap runs inside a counted call"
      (is (= 7 (h 7)))
      (is (= [[:inner "a" 1] :handler] @order)))
    (testing "a refusal is answered by :refused, and the handler never runs"
      (reset! order [])
      (hport/-close! gate "a")
      (is (= {:refused "a" :msg (drain/draining-message "a")} (h 7)))
      (is (= [] @order)))
    (testing "any other failure still throws"
      (drain/open! gate ["a"])
      (let [boom (drain/dispatch-handler "a" (fn [] (throw (ex-info "boom" {})))
                                         {:gate gate :refused (constantly :nope)})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom" (boom)))))
    (testing "without :refused the refusal is thrown"
      (hport/-close! gate "a")
      (let [bare (drain/dispatch-handler "a" identity {:gate gate})]
        (is (drain/draining? (try (bare 1) nil (catch clojure.lang.ExceptionInfo e e))))))))

(deftest a-lifecycle-use-that-outlives-a-forced-unmount-ends-quietly
  (let [mgr {:states (atom {"kept" {:in-flight 1}}) :opts {:now-ms (constantly 42)}}
        end! #'hive-addon.lifecycle/end-use!]
    (testing "the forgotten id neither throws nor comes back"
      (end! mgr "gone")
      (is (not (contains? @(:states mgr) "gone"))))
    (testing "a governed id is still counted down"
      (end! mgr "kept")
      (is (= {:in-flight 0 :last-used-ms 42} (get @(:states mgr) "kept"))))))

(deftest drain-waits-for-the-admitted-call-and-no-longer
  (let [gate  (drain/atom-drain-gate)
        clock (stub-clock)]
    (testing "nothing in flight: no wait at all"
      (let [rep (drain/drain! gate ["a"] clock)]
        (is (= {} (:hot/in-flight rep)))
        (is (zero? (:hot/waited-ms rep)))
        (is (nil? (hs/humanize-errors hs/DrainReport rep)))))
    (testing "a call that finishes during the wait is waited for"
      (let [gate  (drain/atom-drain-gate)
            t     (atom 0)
            _     (hport/-enter! gate "a")
            rep   (drain/drain! gate ["a"] {:drain-ms 1000 :poll-ms 10
                                            :now #(deref t)
                                            :sleep! (fn [ms] (swap! t + ms)
                                                      (when (>= @t 50) (hport/-leave! gate "a")))})]
        (is (= {} (:hot/in-flight rep)))
        (is (= 50 (:hot/waited-ms rep)))))
    (testing "a call that outlives the bound is reported, and the wait stops at the bound"
      (let [gate (drain/atom-drain-gate)
            _    (hport/-enter! gate "a")
            rep  (drain/drain! gate ["a" "b"] (assoc (stub-clock) :drain-ms 200 :poll-ms 30))]
        (is (= {"a" 1} (:hot/in-flight rep)))
        (is (= 200 (:hot/waited-ms rep)))
        (is (false? (hport/-enter! gate "a")) "the gate stays closed until open!")))))

;; =============================================================================
;; eject! drains first
;; =============================================================================

(deftest eject-refuses-while-a-call-runs-and-touches-nothing
  (let [host (mounted-host)
        gate (drain/atom-drain-gate)]
    (hport/-enter! gate "probe.a")
    (let [report (inject/eject! host [spec-a] "probe.a"
                                (merge (stub-clock) {:drain-gate gate :drain-ms 100 :hot-dirs no-dirs}))]
      (is (false? (:ok? report)))
      (is (true? (:hot/busy? report)))
      (is (= {"probe.a" 1} (get-in report [:hot/drain :hot/in-flight])))
      (is (= [] (:hot/torn-down report)))
      (is (some? (port/registered host "probe.a")) "still mounted")
      (is (empty? (fx/events-of :shutdown)) "never shut down")
      (is (true? (hport/-enter! gate "probe.a")) "calls are admitted again")
      (is (nil? (hs/humanize-errors hs/EjectReport report))
          (pr-str (hs/humanize-errors hs/EjectReport report))))
    (testing "plug-out! answers it as :hot/eject-busy"
      (let [res (inject/plug-out! host [spec-a] "probe.a"
                                  (merge (stub-clock) {:drain-gate gate :drain-ms 100 :hot-dirs no-dirs}))]
        (is (= :hot/eject-busy (:error res)))
        (is (string? (:message res)))))))

(deftest eject-forced-goes-ahead-and-says-so
  (let [host (mounted-host)
        gate (drain/atom-drain-gate)]
    (hport/-enter! gate "probe.a")
    (let [report (inject/eject! host [spec-a] "probe.a"
                                (merge (stub-clock) {:drain-gate gate :drain-ms 100 :force? true
                                                     :hot-dirs no-dirs}))]
      (is (:ok? report) (pr-str (:errors report)))
      (is (true? (:hot/forced? report)))
      (is (= ["probe.a"] (:hot/torn-down report)))
      (is (nil? (port/registered host "probe.a"))))))

(deftest eject-with-nothing-in-flight-drains-at-once-and-reopens
  (let [host   (mounted-host)
        gate   (drain/atom-drain-gate)
        report (inject/eject! host [spec-a] "probe.a"
                              (merge (stub-clock) {:drain-gate gate :hot-dirs no-dirs}))]
    (is (:ok? report) (pr-str (:errors report)))
    (is (= ["probe.a"] (get-in report [:hot/drain :hot/draining])))
    (is (= {} (get-in report [:hot/drain :hot/in-flight])))
    (is (nil? (:hot/forced? report)))
    (is (true? (hport/-enter! gate "probe.a")) "a re-injected addon is admitted again")
    (is (nil? (hs/humanize-errors hs/EjectReport report))
        (pr-str (hs/humanize-errors hs/EjectReport report)))))
