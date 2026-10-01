(ns hive-addon.lifecycle-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.hot :as hot]
            [hive-addon.hot-fixture :as fx]
            [hive-addon.lifecycle :as lc]
            [hive-addon.lifecycle.oracle :as oracle]
            [hive-addon.lifecycle.part :as part]
            [hive-addon.lifecycle.policy :as policy]
            [hive-addon.lifecycle.port :as lport]
            [hive-addon.lifecycle.schema :as ls]
            [hive-addon.lifecycle.store :as store]
            [hive-addon.lifecycle.surface :as surface]
            [hive-addon.mount.port :as mport]
            [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Fixtures
;; =============================================================================

(defrecord ToolAddon [id]
  proto/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] #{:tools})
  (initialize! [_ _] (fx/log! [:init id]) {:success? true :errors []})
  (shutdown! [_] (fx/log! [:shutdown id]) nil)
  (tools [_] [{:name "tooled_ping" :description "ping" :handler (fn [_] :pong)}])
  (schema-extensions [_] [])
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] {}))

(defn make-tooled [_config] (fx/log! [:construct "probe.tooled"]) (->ToolAddon "probe.tooled"))

(use-fixtures :each
  (fn [t]
    (fx/reset-fixture!)
    (part/reset-registry!)
    (oracle/reset-dormancy!)
    (try (t) (finally (lc/uninstall!) (part/reset-registry!)))))

(defn- spec
  [id ctor & {:as extra}]
  (merge {:addon/id id :addon/type :native
          :addon/init-ns "hive-addon.hot-fixture" :addon/init-fn ctor}
         extra))

(def ping-surface {:commands {"code" ["probe"]}})

(defn- clock [] (let [t (atom 1000)] {:t t :now-ms #(deref t)}))

(defn- world
  "A manager over an atom mount host with a controllable clock."
  [specs & {:as opts}]
  (let [{:keys [t now-ms]} (clock)
        mh   (mport/atom-mount-host)
        host (lc/mount-host mh)
        mgr  (lc/manager (merge {:host host :specs specs :now-ms now-ms
                                 :reload-ns! (fn [_ _] nil)}
                                opts))]
    {:mgr mgr :host host :mh mh :t t}))

(defn- constructed [id] (count (filter #(= id (second %)) (fx/events-of :construct))))

;; =============================================================================
;; Policy (pure)
;; =============================================================================

(deftest precedence-is-override-then-manifest-then-defaults
  (let [s (spec "a" "make-a" :addon/lifecycle {:policy :lazy :idle-ms 5})]
    (is (= {:policy :lazy :idle-ms 5} (policy/resolve-lifecycle s nil nil)))
    (is (= {:policy :pinned :idle-ms 5} (policy/resolve-lifecycle s {:policy :eager} {:policy :pinned})))
    (is (= {:policy :eager :idle-ms policy/default-idle-ms}
           (policy/resolve-lifecycle (spec "b" "make-b") nil nil)))))

(deftest a-key-a-layer-cannot-honour-is-ignored
  (let [s (spec "a" "make-a" :addon/lifecycle {:policy :sleepy :idle-ms -3})]
    (is (= {:policy :lazy :idle-ms 7} (policy/resolve-lifecycle s {:policy :lazy :idle-ms 7} nil)))))

(deftest boot-downgrades-what-cannot-stay-dormant
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :lazy})
               (spec "probe.b" "make-b" :addon/dependencies #{"probe.a"})
               (spec "probe.c" "make-c" :addon/lifecycle {:policy :lazy})
               (spec "probe.d" "make-shared" :addon/lifecycle {:policy :lazy})]
        lcs   (policy/resolve-all specs {})
        out   (policy/boot-partition specs lcs #{"probe.a" "probe.c"})]
    (is (= ["probe.a" "probe.b" "probe.d"] (:eager out)))
    (is (= ["probe.c"] (:dormant out)))
    (is (= {"probe.a" :required-by-eager "probe.d" :no-surface} (:downgraded out)))))

(deftest a-sweep-frees-a-chain-dependents-first
  (let [specs  [(spec "a" "make-a") (spec "b" "make-b" :addon/dependencies #{"a"})
                (spec "c" "make-c" :addon/dependencies #{"b"})]
        lazy   {:policy :lazy :idle-ms 10}
        st     (fn [id used inflight] {:addon/id id :phase :active :lifecycle lazy
                                       :last-used-ms used :in-flight inflight
                                       :activations 1 :evictions 0})]
    (is (= ["c" "b" "a"]
           (:evict (policy/sweep-plan 100 specs {"a" (st "a" 0 0) "b" (st "b" 0 0) "c" (st "c" 0 0)}))))
    (testing "a busy dependent keeps its whole dependency chain"
      (let [plan (policy/sweep-plan 100 specs {"a" (st "a" 0 0) "b" (st "b" 0 0) "c" (st "c" 0 1)})]
        (is (= [] (:evict plan)))
        (is (= {"a" :dependent-active "b" :dependent-active "c" :in-flight} (:kept plan)))))
    (testing "a fresh addon is kept"
      (is (= :fresh (get-in (policy/sweep-plan 5 specs {"a" (st "a" 0 0)}) [:kept "a"]))))))

(def gen-world
  "A random DAG of up to 7 addons with random phases, uses and policies."
  (gen/let [n     (gen/choose 1 7)
            deps  (gen/vector (gen/vector (gen/choose 0 6) 0 3) n)
            phases (gen/vector (gen/elements [:active :active :dormant]) n)
            used  (gen/vector (gen/choose 0 100) n)
            busy  (gen/vector (gen/frequency [[4 (gen/return 0)] [1 (gen/return 1)]]) n)
            pols  (gen/vector (gen/elements [:lazy :lazy :eager :pinned]) n)]
    (let [ids   (mapv #(str "x" %) (range n))
          specs (mapv (fn [i] (spec (ids i) "make-a"
                                    :addon/dependencies (into #{} (comp (filter #(< % i)) (map ids))
                                                              (deps i))))
                      (range n))
          states (into {} (map (fn [i] [(ids i) {:addon/id (ids i) :phase (phases i)
                                                 :lifecycle {:policy (pols i) :idle-ms 50}
                                                 :last-used-ms (used i) :in-flight (busy i)
                                                 :activations 0 :evictions 0}]))
                       (range n))]
      {:specs specs :states states})))

(defspec evicting-in-plan-order-never-strands-a-live-dependent 200
  (prop/for-all [{:keys [specs states]} gen-world]
    (let [plan (policy/sweep-plan 120 specs states)]
      (loop [active (into #{} (comp (filter #(= :active (:phase (val %)))) (map key)) states)
             [id & more] (:evict plan)]
        (if (nil? id)
          (ls/validate ls/SweepPlan plan)
          (let [st (get states id)]
            (and (= :lazy (get-in st [:lifecycle :policy]))
                 (zero? (:in-flight st))
                 (>= (- 120 (:last-used-ms st)) 50)
                 (not-any? active (disj (policy/dependent-closure specs #{id}) id))
                 (recur (disj active id) more))))))))

;; =============================================================================
;; Boundary
;; =============================================================================

(deftest a-lazy-addon-stays-dormant-behind-stubs-until-used
  (let [specs [(spec "probe.a" "make-a")
               (spec "probe.b" "make-b" :addon/dependencies #{"probe.a"}
                     :addon/lifecycle {:policy :lazy} :addon/surface ping-surface)]
        {:keys [mgr host]} (world specs)
        boot (lc/boot! mgr)]
    (is (ls/validate ls/BootReport boot))
    (is (= ["probe.a"] (:eager boot)))
    (is (= :dormant (lc/phase mgr "probe.b")))
    (is (zero? (constructed "probe.b")))
    (is (= {"code" ["probe"]} (get-in @(:stubs host) ["probe.b" :surface :commands])))
    (testing "the stub's activate! mounts it and withdraws the stubs"
      (let [rep ((get-in @(:stubs host) ["probe.b" :activate!]))]
        (is (ls/validate ls/ActivationReport rep))
        (is (:ok? rep))
        (is (= ["probe.b"] (:activated rep)))
        (is (= :active (lc/phase mgr "probe.b")))
        (is (not (contains? @(:stubs host) "probe.b")))
        (is (= 1 (constructed "probe.b")))))))

(deftest activation-mounts-missing-dependencies-first
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :lazy} :addon/surface ping-surface)
               (spec "probe.b" "make-b" :addon/dependencies #{"probe.a"}
                     :addon/lifecycle {:policy :lazy} :addon/surface ping-surface)]
        {:keys [mgr]} (world specs)]
    (lc/boot! mgr)
    (is (= ["probe.a" "probe.b"] (:activated (lc/activate! mgr "probe.b"))))
    (is (= [[:init "probe.a" 1 #{}] [:init "probe.b" 1 #{"probe.a"}]] (fx/events-of :init)))))

(deftest concurrent-first-uses-mount-once
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :lazy} :addon/surface ping-surface)]
        {:keys [mgr]} (world specs)]
    (lc/boot! mgr)
    (let [calls (doall (repeatedly 16 #(future (lc/with-use mgr "probe.a" :ran))))]
      (is (every? #(= :ran @%) calls)))
    (is (= 1 (constructed "probe.a")))))

(deftest idle-addons-are-evicted-and-come-back-as-new-instances
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :lazy :idle-ms 100}
                     :addon/surface ping-surface)]
        {:keys [mgr host t]} (world specs)]
    (lc/boot! mgr)
    (lc/with-use mgr "probe.a" :ran)
    (swap! t + 50)
    (is (= [] (:evicted (lc/sweep! mgr))) "fresh")
    (swap! t + 100)
    (let [out (lc/sweep! mgr)]
      (is (= ["probe.a"] (:evicted out)))
      (is (= :dormant (lc/phase mgr "probe.a")))
      (is (= [[:shutdown "probe.a" 1]] (fx/events-of :shutdown)))
      (is (contains? @(:stubs host) "probe.a") "stubs are back"))
    (lc/with-use mgr "probe.a" :ran)
    (is (= 2 (constructed "probe.a")))
    (is (= {:activations 2 :evictions 1}
           (select-keys (lc/state mgr "probe.a") [:activations :evictions])))))

(deftest a-use-in-flight-blocks-eviction
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :lazy :idle-ms 1}
                     :addon/surface ping-surface)]
        {:keys [mgr t]} (world specs)
        gate    (promise)
        started (promise)]
    (lc/boot! mgr)
    (lc/activate! mgr "probe.a")
    (let [call (future (lc/with-use mgr "probe.a" (deliver started true) @gate :done))]
      @started
      (swap! t + 1000)
      (is (= :in-flight (:reason (lc/evict! mgr "probe.a"))))
      (deliver gate true)
      (is (= :done @call))
      (swap! t + 1000)
      (is (:evicted? (lc/evict! mgr "probe.a"))))))

(deftest pinned-and-eager-addons-survive-sweeps
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :pinned :idle-ms 1})
               (spec "probe.b" "make-b" :addon/lifecycle {:idle-ms 1})]
        {:keys [mgr t]} (world specs)]
    (lc/boot! mgr)
    (swap! t + 10000)
    (let [out (lc/sweep! mgr)]
      (is (= [] (:evicted out)))
      (is (= {"probe.a" :pinned "probe.b" :eager} (get-in out [:plan :kept]))))
    (testing "force evicts a pinned addon on request"
      (is (= :pinned (:reason (lc/evict! mgr "probe.a"))))
      (is (:evicted? (lc/evict! mgr "probe.a" {:force? true}))))))

(deftest an-addon-with-an-active-dependent-is-not-evicted
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :lazy :idle-ms 1} :addon/surface ping-surface)
               (spec "probe.b" "make-b" :addon/dependencies #{"probe.a"})]
        {:keys [mgr]} (world specs)]
    (lc/boot! mgr)
    (is (= :dependent-active (:reason (lc/evict! mgr "probe.a" {:force? true}))))))

(deftest a-learned-surface-lets-the-next-boot-stay-lazy
  (let [dir   (str (System/getProperty "java.io.tmpdir") "/hive-addon-lifecycle-" (System/nanoTime))
        s     {:addon/id "probe.tooled" :addon/type :native
               :addon/init-ns "hive-addon.lifecycle-test" :addon/init-fn "make-tooled"
               :addon/lifecycle {:policy :lazy}}
        boot1 (let [{:keys [mgr]} (world [s] :surface-store (store/edn-dir-store dir))]
                [(lc/boot! mgr) mgr])]
    (is (= {"probe.tooled" :no-surface} (:downgraded (first boot1))))
    (is (= [{:name "tooled_ping" :description "ping"}]
           (:tools (lport/-load-surface (store/edn-dir-store dir) "probe.tooled"))))
    (let [{:keys [mgr]} (world [s] :surface-store (store/edn-dir-store dir))
          boot2 (lc/boot! mgr)]
      (is (= ["probe.tooled"] (:dormant boot2)))
      (is (= :learned (:surface/source (lc/state mgr "probe.tooled")))))))

(deftest a-surface-learned-at-boot-restores-the-lazy-policy-in-the-same-run
  (let [dir (str (System/getProperty "java.io.tmpdir") "/hive-addon-lifecycle-" (System/nanoTime))
        s   {:addon/id "probe.tooled" :addon/type :native
             :addon/init-ns "hive-addon.lifecycle-test" :addon/init-fn "make-tooled"
             :addon/lifecycle {:policy :lazy :idle-ms 100}}
        {:keys [mgr t]} (world [s] :surface-store (store/edn-dir-store dir))]
    (is (= {"probe.tooled" :no-surface} (:downgraded (lc/boot! mgr))))
    (is (= :lazy (get-in (lc/state mgr "probe.tooled") [:lifecycle :policy])))
    (is (nil? (:downgraded (lc/state mgr "probe.tooled"))))
    (swap! t + 1000)
    (is (= ["probe.tooled"] (:evicted (lc/sweep! mgr))))))

(deftest a-failed-activation-leaves-stubs-to-retry
  (let [specs [(spec "probe.x" "make-broken" :addon/lifecycle {:policy :lazy} :addon/surface ping-surface)]
        {:keys [mgr host]} (world specs)]
    (lc/boot! mgr)
    (let [rep (lc/activate! mgr "probe.x")]
      (is (not (:ok? rep)))
      (is (seq (:errors rep))))
    (is (= :failed (lc/phase mgr "probe.x")))
    (is (contains? @(:stubs host) "probe.x"))
    (is (thrown? clojure.lang.ExceptionInfo (lc/with-use mgr "probe.x" :never)))))

(deftest a-reload-before-mount-that-fails-refuses-the-activation
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :lazy} :addon/surface ping-surface)]
        {:keys [mgr]} (world specs :reload-ns! (fn [_ _] {:failed 'hive-addon.hot-fixture}))]
    (lc/boot! mgr)
    (is (not (:ok? (lc/activate! mgr "probe.a"))))
    (is (zero? (constructed "probe.a")))
    (is (= :dormant (lc/phase mgr "probe.a")))))

(deftest status-is-wire-data
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :lazy} :addon/surface ping-surface)]
        {:keys [mgr]} (world specs)]
    (lc/boot! mgr)
    (let [st (lc/status mgr)]
      (is (= "dormant" (name (get-in st [:addons "probe.a" :phase]))))
      (is (= st (hive-addon.wire/json-safe st))))))

;; =============================================================================
;; hot reload respects dormancy
;; =============================================================================

(deftest a-reload-cascade-does-not-mount-a-dormant-addon
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :lazy} :addon/surface ping-surface)
               (spec "probe.b" "make-b")]
        {:keys [mgr mh]} (world specs)]
    (lc/boot! mgr)
    (lc/install! mgr)
    (let [rep (hot/reload-addon! mh specs "probe.a" {:reload-ns! (fn [_] {:loaded []})})]
      (is (:ok? rep))
      (is (= ["probe.a"] (:hot/dormant rep)))
      (is (zero? (constructed "probe.a"))))
    (testing "an active addon still reloads, and says which seeds slept"
      (let [rep (hot/reload-seeds! mh specs #{"probe.a" "probe.b"} {:reload-ns! (fn [_] {:loaded []})})]
        (is (= ["probe.a"] (:hot/dormant rep)))
        (is (= 2 (constructed "probe.b")))
        (is (zero? (constructed "probe.a")))))))

;; =============================================================================
;; Parts
;; =============================================================================

(deftest parts-close-when-idle-and-reopen-on-use
  (let [t      (atom 0)
        opened (atom 0)
        closed (atom [])
        p      (part/part {:id :idx :idle-ms 10 :clock #(deref t)
                           :open (fn [] (swap! opened inc) {:gen @opened})
                           :close (fn [r] (swap! closed conj r))})]
    (part/register-part! "probe.a" p)
    (is (= {:gen 1} (part/with-part [r p] r)))
    (swap! t + 5)
    (is (= {} (part/sweep! @t)))
    (swap! t + 10)
    (is (= {"probe.a" [:idx]} (part/sweep! @t)))
    (is (= [{:gen 1}] @closed))
    (is (= {:gen 2} (part/with-part [r p] r)))
    (testing "a part in use is not closed"
      (let [gate (promise) started (promise)
            f    (future (part/with-part [_ p] (deliver started true) @gate))]
        @started
        (swap! t + 100)
        (is (= :in-flight (part/close! p)))
        (deliver gate true) @f))))

(deftest evicting-a-pinned-addons-part-keeps-the-addon-mounted
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :pinned})]
        {:keys [mgr t]} (world specs)
        p (part/part {:id :cache :idle-ms 10 :clock #(deref t) :open (fn [] :warm)})]
    (lc/boot! mgr)
    (part/register-part! "probe.a" p)
    (part/acquire! p)
    (swap! t + 100)
    (let [out (lc/sweep! mgr)]
      (is (= [] (:evicted out)))
      (is (= {"probe.a" [:cache]} (:parts out)))
      (is (= :active (lc/phase mgr "probe.a"))))))

(deftest eviction-closes-the-addons-parts
  (let [specs [(spec "probe.a" "make-a" :addon/lifecycle {:policy :lazy :idle-ms 1} :addon/surface ping-surface)]
        {:keys [mgr t]} (world specs)
        p (part/part {:id :conn :idle-ms 100000 :clock #(deref t) :open (fn [] :conn)})]
    (lc/boot! mgr)
    (lc/activate! mgr "probe.a")
    (part/register-part! "probe.a" p)
    (part/acquire! p)
    (swap! t + 10)
    (is (= [:conn] (:parts-closed (lc/evict! mgr "probe.a"))))
    (is (not (part/open? p)))))

;; =============================================================================
;; Surface
;; =============================================================================

(deftest observed-surface-takes-only-this-addons-commands
  (is (= {:tools [{:name "t"}] :commands {"code" ["carto"]}}
         (surface/observed "hive.carto" [{:name "t" :handler identity}]
                           {"code"     {"carto" {:addon "hive.carto"} "lint" {:addon "other"}}
                            "analysis" {"x" {:addon "other"}}}))))

(deftest a-malformed-declared-surface-is-no-surface
  (is (nil? (surface/declared {:addon/surface {:tools "nope"}})))
  (is (nil? (surface/declared {:addon/surface {:commands {"code" []}}}))))
