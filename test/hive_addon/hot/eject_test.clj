(ns hive-addon.hot.eject-test
  "Plugging an addon OUT (hive-addon.hot.inject/eject!), and the injected-spec
   registry that keeps an injected addon discoverable from any thread and
   governed by the lifecycle manager.

   Every collaborator is reached through its PORT: the mount host is an
   IMountHost wrapped in a RECORDING decorator (so the test sees exactly which
   port calls ran), a host WITHOUT IMountUnregister is a separate record (the
   LSP leg: the caller never learns which it has, the report does), and the
   lifecycle host is the library's own ILifecycleHost over the same port."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.hot.dirs :as dirs]
            [hive-addon.hot.inject :as inject]
            [hive-addon.hot.port :as hport]
            [hive-addon.hot.report :as verdict]
            [hive-addon.hot.schema :as hs]
            [hive-addon.hot.source :as source]
            [hive-addon.hot-fixture :as fx]
            [hive-addon.lifecycle :as lc]
            [hive-addon.lifecycle.oracle :as oracle]
            [hive-addon.lifecycle.part :as part]
            [hive-addon.mount :as mount]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.injected :as injected]
            [hive-addon.mount.port :as port]
            [hive-addon.protocol :as proto]
            [hive-dsl.result :as r])
  (:import [clojure.lang DynamicClassLoader]
           [java.io File]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def fixture-ns "hive-addon.hot-fixture")

(defn spec
  [id init-fn & {:as extra}]
  (merge {:addon/id id
          :addon/type :native
          :addon/init-ns fixture-ns
          :addon/init-fn init-fn}
         extra))

(def spec-a (spec "probe.a" "make-a"))
(def spec-b (spec "probe.b" "make-b" :addon/dependencies #{"probe.a"}))

;; =============================================================================
;; Hosts — through the port, never a concretion
;; =============================================================================

(defrecord RecordingHost [inner calls]
  port/IMountHost
  (register! [this addon]
    (swap! calls conj [:register! (proto/addon-id addon)])
    (port/register! inner addon)
    this)
  (init! [_ id config]
    (swap! calls conj [:init! id])
    (port/init! inner id config))
  (shutdown! [_ id]
    (swap! calls conj [:shutdown! id])
    (port/shutdown! inner id))
  (registered [_ id] (port/registered inner id))

  port/IMountUnregister
  (unregister! [this id]
    (swap! calls conj [:unregister! id])
    (port/unregister! inner id)
    this))

(defn- recording-host [] (->RecordingHost (mount/atom-mount-host) (atom [])))

(defrecord LegacyHost [reg]
  ;; An IMountHost that predates IMountUnregister — every existing host.
  port/IMountHost
  (register! [this addon] (swap! reg assoc (proto/addon-id addon) addon) this)
  (init! [_ id config] (some-> (get @reg id) (proto/initialize! config)))
  (shutdown! [_ id] (some-> (get @reg id) proto/shutdown!) nil)
  (registered [_ id] (get @reg id)))

(defn- calls-of [host kind] (into [] (comp (filter #(= kind (first %))) (map second)) @(:calls host)))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- with-dynamic-loader [f]
  (let [t   (Thread/currentThread)
        old (.getContextClassLoader t)]
    (.setContextClassLoader t (DynamicClassLoader. old))
    (try (f) (finally (.setContextClassLoader t old)))))

(use-fixtures :each
  (fn [f]
    (fx/reset-fixture!)
    (injected/reset-registry!)
    (part/reset-registry!)
    (oracle/reset-dormancy!)
    (try (with-dynamic-loader f)
         (finally (lc/uninstall!) (injected/reset-registry!) (part/reset-registry!)))))

(defn- delete-tree! [^File f]
  (when (.isDirectory f) (run! delete-tree! (.listFiles f)))
  (.delete f))

(defn- unload-ns! [sym]
  (remove-ns sym)
  (dosync (alter @#'clojure.core/*loaded-libs* disj sym)))

(defn- fresh-project!
  [id ns-str deps & {:keys [lifecycle]}]
  (let [root (doto (io/file (System/getProperty "java.io.tmpdir")
                            (str "hive-addon-eject-" (System/nanoTime)))
               .mkdirs)
        src  (io/file root "src" (str (source/ns->base-path ns-str) ".clj"))
        man  (io/file root "resources" "META-INF" "hive-addons" (str id ".edn"))]
    (unload-ns! (symbol ns-str))
    (.mkdirs (.getParentFile src))
    (.mkdirs (.getParentFile man))
    (spit (io/file root "deps.edn") (pr-str {:paths ["src" "resources"]}))
    (spit man (pr-str (cond-> {:addon/id id
                               :addon/type :native
                               :addon/init-ns ns-str
                               :addon/init-fn "make"
                               :addon/dependencies deps}
                        lifecycle (assoc :addon/lifecycle lifecycle))))
    (spit src (str "(ns " ns-str " (:require [hive-addon.hot-fixture :as fx]))\n"
                   "(defn make [config] (fx/make-shared (assoc config :probe/id \"" id "\")))\n"))
    root))

(defn- mount-into! [host specs]
  (mount/mount! (mount/solve specs) host)
  host)

;; =============================================================================
;; The port: IMountUnregister is optional (ISP) and substitutable (LSP)
;; =============================================================================

(deftest a-host-without-unregister-still-loads-and-is-reported-unsupported
  (let [legacy (->LegacyHost (atom {}))]
    (is (satisfies? port/IMountHost legacy))
    (is (not (satisfies? port/IMountUnregister legacy)))
    (is (= {:unregistered [] :unsupported ["x"] :errors []}
           (boundary/unregister! legacy ["x"])))))

(deftest the-atom-host-forgets-an-unregistered-addon
  (let [host (mount-into! (mount/atom-mount-host) [spec-a])]
    (is (some? (port/registered host "probe.a")))
    (is (= {:unregistered ["probe.a"] :unsupported [] :errors []}
           (boundary/unregister! host ["probe.a"])))
    (is (nil? (port/registered host "probe.a")))))

;; =============================================================================
;; S9 — the injected-spec registry
;; =============================================================================

(deftest union-specs-lets-the-classpath-win-a-tie
  (is (= [{:addon/id "a" :v :cp} {:addon/id "b" :v :inj}]
         (injected/union-specs [{:addon/id "a" :v :cp}]
                               [{:addon/id "a" :v :inj} {:addon/id "b" :v :inj}]))))

(deftest an-injected-addon-is-discovered-from-a-thread-that-never-saw-its-loader
  (let [root (fresh-project! "probe.inj-disc" "probe.inj-disc-one" #{})]
    (try
      (let [host   (mount-into! (mount/atom-mount-host) [spec-a])
            report (inject/inject! host [spec-a] (str root) {:hot? false})]
        (is (:ok? report) (pr-str (:errors report)))
        (is (= ["probe.inj-disc"] (:hot/remembered report)))
        (testing "a thread on the SYSTEM loader cannot see the manifest, yet discovery does"
          (let [seen (promise)
                t    (doto (Thread. #(deliver seen (mapv :addon/id (:specs (boundary/discover-specs)))))
                       (.setContextClassLoader (ClassLoader/getSystemClassLoader)))]
            (.start t) (.join t 10000)
            (is (some #{"probe.inj-disc"} (deref seen 1000 [])))))
        (testing "an explicit classloader asks for exactly that loader's manifests"
          (is (not-any? #{"probe.inj-disc"}
                        (map :addon/id (:specs (boundary/discover-specs
                                                (ClassLoader/getSystemClassLoader)))))))
        (is (nil? (hs/humanize-errors hs/InjectReport report))
            (pr-str (hs/humanize-errors hs/InjectReport report))))
      (finally
        (unload-ns! 'probe.inj-disc-one)
        (delete-tree! root)))))

(deftest an-injected-addon-is-adopted-by-the-installed-lifecycle-manager
  (let [root (fresh-project! "probe.inj-lc" "probe.inj-lc-one" #{}
                             :lifecycle {:policy :pinned})]
    (try
      (let [mh   (mount-into! (mount/atom-mount-host) [spec-a])
            mgr  (lc/manager {:host (lc/mount-host mh) :specs [spec-a]
                              :reload-ns! (fn [_ _] nil)})]
        (lc/boot! mgr {:mount-eager? false})
        (lc/install! mgr)
        (let [report (inject/inject! mh [spec-a] (str root) {:hot? false})]
          (is (:ok? report) (pr-str (:errors report)))
          (is (= ["probe.inj-lc"] (:hot/adopted report)))
          (is (= :active (lc/phase mgr "probe.inj-lc")))
          (testing "under its MANIFEST policy"
            (is (= :pinned (get-in (lc/state mgr "probe.inj-lc") [:lifecycle :policy])))))
        (testing "eject! reverses the adoption"
          (let [out (inject/eject! mh (into [spec-a] (injected/injected-specs)) "probe.inj-lc" {})]
            (is (:ok? out) (pr-str (:errors out)))
            (is (= ["probe.inj-lc"] (:hot/ungoverned out)))
            (is (nil? (lc/state mgr "probe.inj-lc"))))))
      (finally
        (unload-ns! 'probe.inj-lc-one)
        (delete-tree! root)))))

;; =============================================================================
;; S8 — eject!
;; =============================================================================

(deftest ejecting-an-injected-addon-removes-it-and-reports-what-stays
  (let [root (fresh-project! "probe.plug" "probe.plug-one" #{"probe.a"})]
    (try
      (let [host   (mount-into! (recording-host) [spec-a])
            _      (inject/inject! host [spec-a] (str root) {:hot? false})
            specs  (:specs (boundary/discover-specs))
            live   (filterv #(port/registered host (:addon/id %)) specs)
            report (inject/eject! host live (str root) {})]
        (is (:ok? report) (pr-str (:errors report)))
        (testing "the path resolves to what was injected from it"
          (is (= ["probe.plug"] (:hot/ejected report))))
        (testing "torn down, then unregistered, through the port"
          (is (= ["probe.plug"] (:hot/torn-down report)))
          (is (= ["probe.plug"] (calls-of host :shutdown!)))
          (is (= ["probe.plug"] (calls-of host :unregister!)))
          (is (= ["probe.plug"] (:hot/unregistered report)))
          (is (nil? (port/registered host "probe.plug"))))
        (testing "its dependency is untouched"
          (is (some? (port/registered host "probe.a")))
          (is (not-any? #{"probe.a"} (calls-of host :shutdown!))))
        (testing "forgotten by the registry, so discovery stops seeing it"
          (is (= ["probe.plug"] (:hot/forgotten report)))
          (is (nil? (injected/entry "probe.plug"))))
        (testing "what cannot be removed is REPORTED"
          (is (= 2 (count (:hot/classpath-retained report))))
          (is (= ["probe.plug-one"] (:hot/namespaces-retained report))))
        (is (true? (:teardown/data-preserved? report)))
        (is (nil? (hs/humanize-errors hs/EjectReport report))
            (pr-str (hs/humanize-errors hs/EjectReport report))))
      (finally
        (unload-ns! 'probe.plug-one)
        (delete-tree! root)))))

(deftest ejecting-through-a-legacy-host-says-the-entry-stays
  (let [host   (mount-into! (->LegacyHost (atom {})) [spec-a])
        report (inject/eject! host [spec-a] "probe.a" {})]
    (is (:ok? report) (pr-str (:errors report)))
    (is (= ["probe.a"] (:hot/torn-down report)))
    (is (= [] (:hot/unregistered report)))
    (is (= ["probe.a"] (:hot/unsupported report)))))

(deftest ejecting-an-addon-with-a-mounted-dependent-is-refused-unless-cascaded
  (let [host  (mount-into! (recording-host) [spec-a spec-b])
        specs [spec-a spec-b]]
    (let [report (inject/eject! host specs "probe.a" {})]
      (is (false? (:ok? report)))
      (is (true? (:hot/refused? report)))
      (is (= ["probe.b"] (:hot/blocking report)))
      (is (empty? (calls-of host :shutdown!)) "a refusal touches nothing"))
    (testing ":cascade? tears the dependent down with it and remounts it without the sibling"
      (let [report (inject/eject! host specs "probe.a" {:cascade? true})]
        (is (:ok? report) (pr-str (:errors report)))
        (is (= ["probe.b" "probe.a"] (:hot/torn-down report)))
        (is (= ["probe.b"] (:hot/remounted report)))
        (is (nil? (port/registered host "probe.a")))
        (is (= 2 (:generation (port/registered host "probe.b"))))
        (let [[_ _ _ deps] (last (filter (fn [[_ id _ _]] (= "probe.b" id)) (fx/events-of :init)))]
          (is (= #{} deps) "the remounted dependent no longer receives the ejected sibling"))))))

(deftest ejecting-something-unknown-is-refused-by-value
  (let [report (inject/eject! (mount/atom-mount-host) [] "probe.nope" {})]
    (is (false? (:ok? report)))
    (is (= ["probe.nope"] (:hot/unknown report)))
    (is (seq (:errors report)))))

;; =============================================================================
;; Released dirs go through the IHotDirs port — stubbed here, real below
;; =============================================================================

(defn- recording-dirs
  "An IHotDirs stub answering REMOVE (fn [req] -> Result) and recording every
   request. Reify'd per test, through the port."
  [remove]
  (let [calls (atom [])]
    {:calls calls
     :port  (reify hport/IHotDirs
              (-extend-dirs! [_ req] (swap! calls conj [:extend req])
                (r/ok {:dirs (:dirs req) :added (:dirs req)}))
              (-remove-dirs! [_ req] (swap! calls conj [:remove req])
                (remove req)))}))

(deftest eject-releases-each-owners-claim-through-the-port
  (let [host   (mount-into! (recording-host) [spec-a])
        dirs   (vec (source/watchable-dirs [spec-a]))
        {:keys [calls port]}
        (recording-dirs (fn [{ds :dirs}]
                          (r/ok {:removed [] :kept (vec ds) :absent [] :dirs (vec ds)
                                 :shared (into {} (map (fn [d] [d ["other.owner"]])) ds)})))
        report (inject/eject! host [spec-a] "probe.a" {:hot-dirs port})]
    (is (:ok? report) (pr-str (:errors report)))
    (when (seq dirs)
      (testing "one request per ejected owner, naming the owner"
        (is (= [[:remove {:dirs dirs :owner "probe.a"}]] @calls)))
      (testing "a dir another owner claims stays, and the report says who"
        (is (= [] (:hot/dirs-removed report)))
        (is (= (vec (sort dirs)) (:hot/dirs-retained report)))
        (is (= (into {} (map (fn [d] [d ["other.owner"]])) dirs) (:hot/dirs-shared report)))
        (is (re-find #"another owner" (:hot/dirs-reason report)))))
    (is (nil? (hs/humanize-errors hs/EjectReport report))
        (pr-str (hs/humanize-errors hs/EjectReport report)))))

(deftest eject-reports-a-dirs-port-failure-as-retained-with-the-reason
  (let [host   (mount-into! (recording-host) [spec-a])
        dirs   (vec (source/watchable-dirs [spec-a]))
        {:keys [port]} (recording-dirs (fn [_] (r/err :hot/dirs-unavailable {:message "no hive-hot"})))
        report (inject/eject! host [spec-a] "probe.a" {:hot-dirs port})]
    (is (:ok? report) "the addon is out; only the watch stays")
    (when (seq dirs)
      (is (= (vec (sort dirs)) (:hot/dirs-retained report)))
      (is (= "no hive-hot" (:hot/dirs-reason report))))))

(deftest dirs-outcome-reads-the-remove-dirs-report-contract
  (testing "removed by one owner wins over kept by another request"
    (is (= {:removed ["/x"] :retained [] :shared {}}
           (verdict/dirs-outcome [{:dirs ["/x"] :owner "a"} {:dirs ["/x"] :owner "b"}]
                                 [(r/ok {:removed [] :kept ["/x"] :absent [] :dirs ["/x"] :shared {"/x" ["b"]}})
                                  (r/ok {:removed ["/x"] :kept [] :absent [] :dirs [] :shared {}})]))))
  (testing "a core dir is retained with the core reason; absent is neither"
    (let [out (verdict/dirs-outcome [{:dirs ["/core" "/gone"] :owner "a"}]
                                    [(r/ok {:removed [] :kept ["/core"] :absent ["/gone"] :dirs ["/core"] :shared {}})])]
      (is (= [] (:removed out)))
      (is (= ["/core"] (:retained out)))
      (is (re-find #"initial init" (:reason out)))))
  (testing "an err answer retains every dir of its request"
    (is (= {:removed [] :retained ["/y"] :shared {} :reason "boom"}
           (verdict/dirs-outcome [{:dirs ["/y"] :owner "a"}] [(r/err :x {:message "boom"})])))))

;; =============================================================================
;; plug-out! — unsafe ejects refused loudly, as Result errors
;; =============================================================================

(deftest plug-out-refuses-an-unsafe-eject-as-an-err-result
  (let [host (mount-into! (recording-host) [spec-a spec-b])
        stub (:port (recording-dirs (fn [_] (r/ok {:removed [] :kept [] :absent [] :dirs [] :shared {}}))))
        res  (inject/plug-out! host [spec-a spec-b] "probe.a" {:hot-dirs stub})]
    (is (r/err? res))
    (is (= :hot/eject-refused (:error res)))
    (is (= ["probe.b"] (:hot/blocking res)))
    (is (string? (:message res)))
    (is (empty? (calls-of host :shutdown!)) "a refusal touches nothing"))
  (testing "unknown target"
    (let [res (inject/plug-out! (mount/atom-mount-host) [] "probe.nope" {})]
      (is (= :hot/eject-unknown (:error res)))))
  (testing "a safe eject is ok, with the report"
    (let [host (mount-into! (recording-host) [spec-a])
          stub (:port (recording-dirs (fn [{ds :dirs}] (r/ok {:removed (vec ds) :kept [] :absent [] :dirs [] :shared {}}))))
          res  (inject/plug-out! host [spec-a] "probe.a" {:hot-dirs stub})]
      (is (r/ok? res))
      (is (= ["probe.a"] (:hot/ejected (:ok res))))
      (is (true? (:teardown/data-preserved? (:ok res)))))))

;; =============================================================================
;; The adapter against the real hive-hot — the report contract, owner claims
;; =============================================================================

(deftest the-hive-hot-adapter-answers-the-remove-dirs-report-contract
  (let [tmp     (fn [tag] (doto (io/file (System/getProperty "java.io.tmpdir")
                                         (str "hive-addon-dirs-" tag "-" (System/nanoTime)))
                            .mkdirs))
        core    (tmp "core")
        plugged (tmp "plugged")
        [c p]   [(str core) (str plugged)]
        port    (dirs/hot-dirs)]
    (try
      (if (try (requiring-resolve 'hive-hot.core/remove-dirs!)
               (catch Throwable _hive-hot-absent-means-degraded-path nil))
        (let [init!   (requiring-resolve 'hive-hot.core/init!)
              reset!* (requiring-resolve 'hive-hot.core/reset-all!)
              Report  @(requiring-resolve 'hive-hot.schema/RemoveDirsReport)
              valid?  (requiring-resolve 'malli.core/validate)]
          (try
            (init! {:dirs [c]})
            (is (r/ok? (hport/-extend-dirs! port {:dirs [p] :owner "x"})))
            (is (r/ok? (hport/-extend-dirs! port {:dirs [p] :owner "y"})))
            (testing "a dir another owner claims is kept and shared"
              (let [out (hport/-remove-dirs! port {:dirs [p] :owner "x"})]
                (is (r/ok? out))
                (is (valid? Report (:ok out)) (pr-str out))
                (is (= [p] (:kept (:ok out))))
                (is (= {p ["y"]} (:shared (:ok out))))))
            (testing "the last owner's release removes it; a core dir is kept"
              (let [out (:ok (hport/-remove-dirs! port {:dirs [p c] :owner "y"}))]
                (is (valid? Report out) (pr-str out))
                (is (= [p] (:removed out)))
                (is (= [c] (:kept out)))))
            (testing "idempotent: a second release finds it absent"
              (is (= [p] (:absent (:ok (hport/-remove-dirs! port {:dirs [p] :owner "y"}))))))
            (finally (reset!*))))
        (testing "a hive-hot without remove-dirs! answers an err naming why"
          (let [out (hport/-remove-dirs! port {:dirs [p] :owner "x"})]
            (is (r/err? out))
            (is (string? (:message out))))))
      (finally (delete-tree! core) (delete-tree! plugged)))))
