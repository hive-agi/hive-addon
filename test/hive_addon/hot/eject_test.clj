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
            [hive-addon.hot.inject :as inject]
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
            [hive-addon.protocol :as proto])
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
