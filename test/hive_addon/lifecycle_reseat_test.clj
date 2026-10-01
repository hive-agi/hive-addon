(ns hive-addon.lifecycle-reseat-test
  "reseat-host!: govern the same addon state through a new ILifecycleHost.

   Hosts are wrapped in a RECORDING decorator tagged with a generation, so a
   test asserts which host every call reached without redefining anything."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.hot-fixture :as fx]
            [hive-addon.lifecycle :as lc]
            [hive-addon.lifecycle.oracle :as oracle]
            [hive-addon.lifecycle.part :as part]
            [hive-addon.lifecycle.port :as lport]
            [hive-addon.lifecycle.schema :as ls]
            [hive-addon.mount.port :as mport]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each
  (fn [t]
    (fx/reset-fixture!)
    (part/reset-registry!)
    (oracle/reset-dormancy!)
    (try (t) (finally (lc/uninstall!) (part/reset-registry!)))))

;; =============================================================================
;; Recording decorator (DIP: a port wrapper, no redefinition)
;; =============================================================================

(defrecord RecordingHost [inner tag log]
  lport/ILifecycleHost
  (-mount! [_ specs peers]
    (swap! log conj [tag :mount (mapv :addon/id specs)])
    (lport/-mount! inner specs peers))
  (-unmount! [_ id]
    (swap! log conj [tag :unmount id])
    (lport/-unmount! inner id))
  (-install-stubs! [_ id s activate!]
    (swap! log conj [tag :install-stubs id])
    (lport/-install-stubs! inner id s activate!))
  (-remove-stubs! [_ id]
    (swap! log conj [tag :remove-stubs id])
    (lport/-remove-stubs! inner id))
  (-observe-surface [_ id]
    (swap! log conj [tag :observe id])
    (lport/-observe-surface inner id)))

(defn- recording
  "A recording host of generation TAG over the shared mount host MH."
  [mh tag log]
  (->RecordingHost (lc/mount-host mh) tag log))

(defn- stubs-of [rh] @(:stubs (:inner rh)))

(def surface {:commands {"code" ["probe"]}})

(defn- spec [id ctor]
  {:addon/id id :addon/type :native
   :addon/init-ns "hive-addon.hot-fixture" :addon/init-fn ctor
   :addon/lifecycle {:policy :lazy :idle-ms 10}
   :addon/surface surface})

(def specs [(spec "probe.a" "make-a") (spec "probe.b" "make-b") (spec "probe.c" "make-c")])

(defn- world []
  (let [mh  (mport/atom-mount-host)
        log (atom [])
        h0  (recording mh 0 log)
        mgr (lc/manager {:host h0 :specs specs :reload-ns! (fn [_ _] nil)})]
    (lc/boot! mgr)
    {:mgr mgr :mh mh :log log :h0 h0}))

(defn- tags [log] (set (map first log)))

;; =============================================================================
;; Examples
;; =============================================================================

(deftest reseat-rearms-dormant-stubs-on-the-new-host-and-never-calls-the-old
  (let [{:keys [mgr mh log h0]} (world)
        _   (lc/install! mgr)
        _   (lc/activate! mgr "probe.a")
        h1  (recording mh 1 log)
        _   (reset! log [])
        rep (lc/reseat-host! mgr h1)]
    (is (ls/validate ls/ReseatReport rep))
    (is (= {:ok? true :reseated? true :installed? true :sweeper-moved? false
            :rearmed ["probe.b" "probe.c"]}
           rep))
    (is (= #{1} (tags @log)) "the old host is not called by the reseat")
    (is (= #{"probe.b" "probe.c"} (set (keys (stubs-of h1)))))
    (is (identical? h1 (:host (lc/current mgr))))
    (is (identical? (lc/current mgr) (lc/installed-manager)))
    (testing "a stub armed on the OLD host, before the reseat, activates on the new host"
      (reset! log [])
      (let [rep ((get-in (stubs-of h0) ["probe.b" :activate!]))]
        (is (:ok? rep))
        (is (= :active (lc/phase mgr "probe.b")))
        (is (= #{1} (tags @log)))))
    (testing "a stub armed on the new host activates too"
      (reset! log [])
      (is (:ok? ((get-in (stubs-of h1) ["probe.c" :activate!]))))
      (is (= #{1} (tags @log))))
    (testing "eviction through the OLD manager value reaches the new host"
      (reset! log [])
      (is (:evicted? (lc/evict! mgr "probe.a")))
      (is (= #{1} (tags @log)))
      (is (contains? (stubs-of h1) "probe.a")))))

(deftest reseat-accepts-a-host-fn-of-the-old-host
  (let [{:keys [mgr mh log h0]} (world)
        seen (atom nil)
        rep  (lc/reseat-host! mgr (fn [old] (reset! seen old) (recording mh 1 log)))]
    (is (:reseated? rep))
    (is (identical? h0 @seen))
    (is (= 1 (:tag (:host (lc/current mgr)))))))

(deftest a-failing-or-wrong-host-fn-leaves-the-old-host-seated
  (let [{:keys [mgr h0]} (world)]
    (doseq [bad [(fn [_] (throw (ex-info "boom" {}))) (fn [_] :not-a-host)]]
      (let [rep (lc/reseat-host! mgr bad)]
        (is (ls/validate ls/ReseatReport rep))
        (is (false? (:ok? rep)))
        (is (false? (:reseated? rep)))
        (is (seq (:errors rep)))
        (is (identical? h0 (:host (lc/current mgr))))))))

(deftest reseat-does-not-install-over-a-foreign-installed-manager
  (let [{:keys [mgr mh log]} (world)
        other (:mgr (world))]
    (lc/install! other)
    (let [rep (lc/reseat-host! mgr (recording mh 1 log))]
      (is (false? (:installed? rep)))
      (is (identical? other (lc/installed-manager))))))

(deftest reseat-moves-a-running-sweeper
  (let [{:keys [mgr mh log]} (world)]
    (lc/start-sweeper! mgr {:interval-ms 60000})
    (let [before @(:sweeper mgr)
          rep    (lc/reseat-host! mgr (recording mh 1 log))
          after  @(:sweeper (lc/current mgr))]
      (try
        (is (:sweeper-moved? rep))
        (is (some? after))
        (is (not (identical? (:executor before) (:executor after))))
        (is (.isShutdown ^java.util.concurrent.ExecutorService (:executor before)))
        (is (= 60000 (:interval-ms after)))
        (finally (lc/stop-sweeper! (lc/current mgr)))))))

;; =============================================================================
;; Property: activation racing reseat never uses a replaced host
;; =============================================================================

(defn- churn!
  "Activate and force-evict ID through the ORIGINAL manager value until STOP."
  [mgr id stop]
  (future
    (while (not @stop)
      (lc/activate! mgr id)
      (lc/evict! mgr id {:force? true}))))

(defn- stale-calls
  "Calls logged, after the reseat to generation G returned (log index AT), on a
   host of an older generation."
  [log reseats]
  (for [[g at] reseats
        [tag :as entry] (drop at log)
        :when (< tag g)]
    entry))

(defspec activation-racing-reseat-never-reaches-a-replaced-host 15
  (prop/for-all [n-reseats (gen/choose 1 4)
                 workers   (gen/choose 1 3)]
    (fx/reset-fixture!)
    (lc/uninstall!)
    (let [{:keys [mgr mh log]} (world)
          stop    (atom false)
          futs    (mapv #(churn! mgr (:addon/id (specs %)) stop) (range workers))
          reseats (vec (for [g (range 1 (inc n-reseats))]
                         (do (Thread/sleep 5)
                             (let [rep (lc/reseat-host! mgr (recording mh g log))]
                               [g (count @log) rep]))))]
      (Thread/sleep 10)
      (reset! stop true)
      (run! deref futs)
      (and (every? (comp :reseated? #(nth % 2)) reseats)
           (empty? (stale-calls @log (map (juxt first second) reseats)))
           (= n-reseats (:tag (:host (lc/current mgr))))))))
