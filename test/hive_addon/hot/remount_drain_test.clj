(ns hive-addon.hot.remount-drain-test
  "A remount drains the slice's calls in flight before it tears anything down,
   through the same drain eject! runs (hive-addon.hot.drain/drained!).

   Seen live: `hot reload` remounted hive.compose while one of its tool calls
   (a ~3.5 min claim) was still running on the old instance; the call finished
   on the dead instance and its effects never reached the new one.

   The blocking test is real time on purpose: a tool call parked on a latch in
   one thread, the reload in another, and the assertion that the old instance is
   not shut down until the call returns."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.hot :as hot]
            [hive-addon.hot.drain :as drain]
            [hive-addon.hot.port :as hport]
            [hive-addon.hot.schema :as hs]
            [hive-addon.hot.strategy :as strategy]
            [hive-addon.hot-fixture :as fx]
            [hive-addon.mount :as mount]
            [hive-addon.mount.port :as port])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each
  (fn [f]
    (fx/reset-fixture!)
    (strategy/reset-strategies!)
    (hot/clear-reload-failures!)
    (try (f) (finally (strategy/reset-strategies!) (hot/clear-reload-failures!)))))

(def spec-a {:addon/id "probe.a" :addon/type :native
             :addon/init-ns "hive-addon.hot-fixture" :addon/init-fn "make-a"})

(def ^:private no-ns-reload
  "Skip the namespace pass: what is under test is the teardown, not the load."
  {:reload-ns! (fn [ns-strs] {:loaded (mapv symbol ns-strs)})})

(defn- mounted-host []
  (doto (mount/atom-mount-host) (->> (mount/mount! (mount/solve [spec-a])))))

(defn- stub-clock []
  (let [t (atom 0)]
    {:now #(deref t) :sleep! #(swap! t + %)}))

(defn- schema-errors [report]
  (hs/humanize-errors hs/RemountReport report))

(deftest a-remount-waits-for-the-call-in-flight-before-teardown
  (let [host     (mounted-host)
        gate     (drain/atom-drain-gate)
        entered  (CountDownLatch. 1)
        release  (CountDownLatch. 1)
        call     (drain/tracked gate "probe.a"
                                (fn [] (.countDown entered)
                                  (.await release 10 TimeUnit/SECONDS)
                                  ;; How many shutdowns happened while the call ran.
                                  (count (fx/events-of :shutdown))))
        in-call  (future (call))
        _        (is (.await entered 5 TimeUnit/SECONDS) "the call is running")
        reload   (future (hot/reload-addon! host [spec-a] "probe.a"
                                            (assoc no-ns-reload
                                                   :drain-gate gate :drain-ms 10000 :poll-ms 5)))]
    (Thread/sleep 200)
    (testing "while the call runs nothing is torn down and new calls are refused"
      (is (not (realized? reload)) "the reload is waiting on the drain")
      (is (empty? (fx/events-of :shutdown)) "the old instance is still up")
      (is (false? (hport/-enter! gate "probe.a")) "the gate is closed to new calls"))
    (.countDown release)
    (let [shutdowns-seen-by-call (deref in-call 5000 ::timeout)
          report                 (deref reload 10000 ::timeout)]
      (is (= 0 shutdowns-seen-by-call) "the call finished on a live instance")
      (is (map? report))
      (is (:ok? report) (pr-str (:errors report)))
      (is (= ["probe.a"] (:hot/torn-down report)))
      (is (= {} (get-in report [:hot/drain :hot/in-flight])) "drained, not forced")
      (is (pos? (get-in report [:hot/drain :hot/waited-ms] 0)) "the wait is reported")
      (is (nil? (:hot/forced? report)))
      (is (= 2 (:generation (port/registered host "probe.a"))) "a new instance is mounted")
      (is (true? (hport/-enter! gate "probe.a")) "calls are admitted again")
      (hport/-leave! gate "probe.a")
      (is (nil? (schema-errors report)) (pr-str (schema-errors report))))))

(deftest a-remount-with-a-call-outliving-the-bound-is-refused-busy
  (let [host (mounted-host)
        gate (drain/atom-drain-gate)]
    (hport/-enter! gate "probe.a")
    (let [report (hot/reload-addon! host [spec-a] "probe.a"
                                    (merge no-ns-reload (stub-clock)
                                           {:drain-gate gate :drain-ms 100}))]
      (is (false? (:ok? report)))
      (is (true? (:hot/busy? report)))
      (is (true? (:hot/refused? report)))
      (is (= {"probe.a" 1} (get-in report [:hot/drain :hot/in-flight])))
      (is (= 100 (get-in report [:hot/drain :hot/waited-ms])))
      (is (= [] (:hot/torn-down report)))
      (is (= [] (:hot/down report)))
      (is (re-find #"in flight" (str (first (:errors report)))))
      (is (empty? (fx/events-of :shutdown)) "never shut down")
      (is (= 1 (:generation (port/registered host "probe.a"))) "the old instance keeps serving")
      (is (empty? (hot/reload-failures)) "a busy refusal is not a load failure")
      (is (nil? (schema-errors report)) (pr-str (schema-errors report))))
    (testing "the gate is re-opened: new calls are admitted"
      (is (true? (hport/-enter! gate "probe.a"))))))

(deftest a-forced-remount-goes-ahead-and-says-so
  (let [host (mounted-host)
        gate (drain/atom-drain-gate)]
    (hport/-enter! gate "probe.a")
    (let [report (hot/reload-addon! host [spec-a] "probe.a"
                                    (merge no-ns-reload (stub-clock)
                                           {:drain-gate gate :drain-ms 100 :force? true}))]
      (is (:ok? report) (pr-str (:errors report)))
      (is (true? (:hot/forced? report)))
      (is (= {"probe.a" 1} (get-in report [:hot/drain :hot/in-flight])) "the cut call is named")
      (is (= ["probe.a"] (:hot/torn-down report)))
      (is (= 2 (:generation (port/registered host "probe.a"))))
      (is (nil? (schema-errors report)) (pr-str (schema-errors report))))))

(deftest a-remount-drains-the-dependents-it-tears-down-too
  (let [spec-b (assoc spec-a :addon/id "probe.b" :addon/init-fn "make-b"
                      :addon/dependencies #{"probe.a"})
        host   (doto (mount/atom-mount-host) (->> (mount/mount! (mount/solve [spec-a spec-b]))))
        gate   (drain/atom-drain-gate)]
    (hport/-enter! gate "probe.b")
    (let [report (hot/reload-addon! host [spec-a spec-b] "probe.a"
                                    (merge no-ns-reload (stub-clock)
                                           {:drain-gate gate :drain-ms 50}))]
      (is (true? (:hot/busy? report)) "a dependent's call blocks the seed's remount")
      (is (= #{"probe.a" "probe.b"} (set (get-in report [:hot/drain :hot/draining]))))
      (is (= {"probe.b" 1} (get-in report [:hot/drain :hot/in-flight])))
      (is (empty? (fx/events-of :shutdown))))))
