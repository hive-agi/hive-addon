(ns hive-addon.hot.atomic-reload-test
  "A reload is all-or-nothing per addon.

   An addon whose code fails to LOAD during a reload (the namespace pass stops,
   or a widened reload drags in an addon whose constructor namespace will not
   compile) is not remounted with half-new code: its previous instance keeps
   serving, the report says FAILED and names it under :hot/reload-failures, and
   the next reload that reaches it is REFUSED without loading anything, until a
   retry is asked for (card 20261118114629-0192640d item 3).

   The fixture addons live in a throwaway source root on a DynamicClassLoader:
   probe.atomic-addon is a real constructor namespace that is then rewritten so
   it no longer compiles, and probe.atomic-broken never compiled at all."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.hot :as hot]
            [hive-addon.hot.failures :as failures]
            [hive-addon.hot.inject :as inject]
            [hive-addon.hot.schema :as hs]
            [hive-addon.hot.strategy :as strategy]
            [hive-addon.hot-fixture :as fx]
            [hive-addon.mount :as mount]
            [hive-addon.mount.port :as port]
            [hive-dsl.result :as r])
  (:import [clojure.lang DynamicClassLoader]
           [java.io File]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def fixture-ns "hive-addon.hot-fixture")
(def atomic-ns "probe.atomic-addon")
(def broken-ns "probe.atomic-broken")

(defn- unload-ns! [sym]
  (remove-ns sym)
  (dosync (alter @#'clojure.core/*loaded-libs* disj sym)))

(defn- delete-tree! [^File f]
  (when (.isDirectory f) (run! delete-tree! (.listFiles f)))
  (.delete f))

(def ^:private good-src
  (str "(ns probe.atomic-addon\n"
       "  (:require [hive-addon.hot-fixture :as fx]))\n"
       "(defn make [config] (fx/make-shared (assoc config :probe/id \"probe.atomic\")))\n"))

(def ^:private bad-src
  ;; Compiles to nothing: the symbol does not resolve, so loading the namespace
  ;; throws a CompilerException after the ns form already ran.
  (str "(ns probe.atomic-addon\n"
       "  (:require [hive-addon.hot-fixture :as fx]))\n"
       "(defn make [config] (no-such-fn-anywhere config))\n"))

(def ^:private broken-src
  (str "(ns probe.atomic-broken)\n"
       "(defn make [config] (also-not-a-fn config))\n"))

(def ^:dynamic *root* nil)

(defn- src-file [rel] (io/file *root* "src" "probe" rel))

(defn- with-project [f]
  (let [t    (Thread/currentThread)
        old  (.getContextClassLoader t)
        root (doto (io/file (System/getProperty "java.io.tmpdir")
                            (str "hive-addon-atomic-" (System/nanoTime)))
               .mkdirs)]
    (.setContextClassLoader t (DynamicClassLoader. old))
    (try
      (fx/reset-fixture!)
      (failures/clear!)
      (strategy/reset-strategies!)
      (run! unload-ns! '[probe.atomic-addon probe.atomic-broken])
      (.mkdirs (io/file root "src" "probe"))
      (binding [*root* root]
        (spit (src-file "atomic_addon.clj") good-src)
        (spit (src-file "atomic_broken.clj") broken-src)
        (is (not (r/err? (inject/extend-classpath! (str (io/file root "src"))))))
        (f))
      (finally
        (failures/clear!)
        (strategy/reset-strategies!)
        (run! unload-ns! '[probe.atomic-addon probe.atomic-broken])
        (delete-tree! root)
        (.setContextClassLoader t old)))))

(use-fixtures :each with-project)

(defn- spec [id init-ns init-fn & {:as extra}]
  (merge {:addon/id id :addon/type :native :addon/init-ns init-ns :addon/init-fn init-fn}
         extra))

(def spec-a (spec "probe.a" fixture-ns "make-a"))
(def spec-b (spec "probe.b" fixture-ns "make-b" :addon/dependencies #{"probe.a"}))
(def spec-atomic (spec "probe.atomic" atomic-ns "make"))

(defn- generation [host id] (:generation (port/registered host id)))

(defn- counting-require-reload
  "The real fallback reloader (`require :reload`, which compiles the file on
   disk), counting its calls."
  [calls]
  (fn [nss]
    (swap! calls inc)
    (#'hot/require-reload! nss)))

(defn- schema-ok [report]
  (is (nil? (hs/humanize-errors hs/RemountReport report))
      (pr-str (hs/humanize-errors hs/RemountReport report))))

;; =============================================================================
;; A constructor namespace that stops compiling
;; =============================================================================

(deftest a-ctor-ns-that-fails-to-compile-keeps-the-old-instance-and-is-quarantined
  (require 'probe.atomic-addon)
  (let [host  (mount/atom-mount-host)
        _     (mount/mount! (mount/solve [spec-atomic]) host)
        old   (port/registered host "probe.atomic")
        calls (atom 0)
        opts  {:reload-ns! (counting-require-reload calls)}]
    (is (= 1 (generation host "probe.atomic")) "precondition: mounted from good code")
    (spit (src-file "atomic_addon.clj") bad-src)
    (let [report (hot/reload-addon! host [spec-atomic] "probe.atomic" opts)]
      (testing "the report says FAILED, plainly"
        (is (false? (:ok? report)))
        (is (true? (:hot/refused? report)))
        (is (some #(re-find #"reload FAILED" %) (:errors report)) (pr-str (:errors report)))
        (is (= :hot/reload-failed (get-in report [:diagnostic :code]))))
      (testing "the old instance keeps serving: nothing torn down, nothing rebuilt"
        (is (empty? (:hot/torn-down report)))
        (is (= [] (:hot/down report)))
        (is (identical? old (port/registered host "probe.atomic")))
        (is (= 1 (generation host "probe.atomic")))
        (is (empty? (fx/events-of :shutdown))))
      (testing "the addon is marked :reload/failed with the error"
        (let [mark (get (:hot/reload-failures report) "probe.atomic")]
          (is (= :ns-load (:reload/phase mark)))
          (is (= atomic-ns (:reload/failed-ns mark)))
          (is (re-find #"no-such-fn-anywhere" (:reload/error mark)) (:reload/error mark)))
        (is (= :reload/failed (get-in (hot/reload-failures) ["probe.atomic" :reload/status])))
        (is (contains? (:hot/reload-failures (hot/status)) "probe.atomic")))
      (schema-ok report))
    (is (= 1 @calls))
    (testing "a second reload is REFUSED with a reason, and loads nothing"
      (let [again (hot/reload-addon! host [spec-atomic] "probe.atomic" opts)]
        (is (false? (:ok? again)))
        (is (true? (:hot/refused? again)))
        (is (= {"probe.atomic" ["probe.atomic"]} (:hot/quarantined again)))
        (is (= :hot/reload-quarantined (get-in again [:diagnostic :code])))
        (is (some #(re-find #"reload REFUSED.*no-such-fn-anywhere" %) (:errors again))
            (pr-str (:errors again)))
        (is (= 1 @calls) "the reloader was not called again")
        (is (identical? old (port/registered host "probe.atomic")))
        (schema-ok again)))
    (testing "the attempt count does not grow while it is refused"
      (is (= 1 (get-in (hot/reload-failures) ["probe.atomic" :reload/attempts]))))
    (testing "once the code is fixed, a retry lifts the mark and remounts"
      (spit (src-file "atomic_addon.clj") good-src)
      (let [fixed (hot/reload-addon! host [spec-atomic] "probe.atomic"
                                     (assoc opts :retry-failed? true))]
        (is (:ok? fixed) (pr-str (:errors fixed)))
        (is (= 2 (generation host "probe.atomic")))
        (is (empty? (hot/reload-failures)))))))

(deftest a-failed-retry-stays-quarantined-and-counts-the-attempt
  (require 'probe.atomic-addon)
  (let [host (mount/atom-mount-host)
        opts {:reload-ns! (counting-require-reload (atom 0))}]
    (mount/mount! (mount/solve [spec-atomic]) host)
    (spit (src-file "atomic_addon.clj") bad-src)
    (hot/reload-addon! host [spec-atomic] "probe.atomic" opts)
    (let [retry (hot/reload-addon! host [spec-atomic] "probe.atomic"
                                   (assoc opts :retry-failed? true))]
      (is (false? (:ok? retry)))
      (is (nil? (:hot/quarantined retry)) "a retry runs the load again")
      (is (= 2 (get-in (hot/reload-failures) ["probe.atomic" :reload/attempts])))
      (is (= 1 (generation host "probe.atomic"))))))

;; =============================================================================
;; A widened reload that reaches a broken addon
;; =============================================================================

(deftest a-widened-reload-whose-dragged-ctor-ns-will-not-load-remounts-nothing
  (testing "the reload of probe.a loads probe.out's constructor namespace too,
            and that namespace does not compile: the whole slice is refused
            before teardown, nobody runs half-new code, and probe.out is
            quarantined"
    (let [good-out (spec "probe.out" fixture-ns "make-shared"
                         :addon/config {:probe/id "probe.out"})
          host     (mount/atom-mount-host)
          _        (mount/mount! (mount/solve [spec-a spec-b good-out]) host)
          ;; The same addon as the host now knows it: its constructor
          ;; namespace is the one the reload dragged in, which will not load.
          specs    [spec-a spec-b (assoc good-out :addon/init-ns broken-ns :addon/init-fn "make")]
          report   (hot/reload-addon! host specs "probe.a"
                                      {:reload-ns! (fn [_] {:loaded [fixture-ns broken-ns]})})]
      (is (false? (:ok? report)))
      (is (true? (:hot/refused? report)))
      (is (= #{"probe.out"} (:hot/widened report)))
      (is (empty? (:hot/torn-down report)))
      (is (= [1 1 1] (mapv #(generation host %) ["probe.a" "probe.b" "probe.out"])))
      (is (= :preflight (get-in report [:hot/reload-failures "probe.out" :reload/phase])))
      (is (= ["probe.out"] (keys (:hot/reload-failures report)))
          "only the addon whose code would not load is quarantined")
      (is (some #(re-find #"probe.out: reload FAILED at preflight" %) (:errors report))
          (pr-str (:errors report)))
      (schema-ok report)
      (testing "a later reload of the quarantined addon is refused"
        (let [again (hot/reload-addon! host specs "probe.out"
                                       {:reload-ns! (fn [_] (throw (ex-info "must not run" {})))})]
          (is (true? (:hot/refused? again)))
          (is (= {"probe.out" ["probe.out"]} (:hot/quarantined again))))))))

(deftest a-failed-pass-marks-every-addon-whose-ctor-ns-it-touched
  (testing "the reload of probe.a unloaded probe.atomic's constructor namespace
            and then failed at it: probe.atomic is half-loaded although nobody
            asked to reload it"
    (require 'probe.atomic-addon)
    (let [specs  [spec-a spec-b spec-atomic]
          host   (mount/atom-mount-host)
          _      (mount/mount! (mount/solve specs) host)
          report (hot/reload-addon! host specs "probe.a"
                                    {:reload-ns! (fn [_] {:loaded [fixture-ns]
                                                          :unloaded [(symbol atomic-ns)]
                                                          :failed (symbol atomic-ns)
                                                          :error "Syntax error compiling at (probe/atomic_addon.clj:3:1)"})})]
      (is (false? (:ok? report)))
      (is (= #{"probe.a" "probe.atomic"} (set (keys (:hot/reload-failures report)))))
      (is (= [1 1 1] (mapv #(generation host %) ["probe.a" "probe.b" "probe.atomic"])))
      (schema-ok report)
      (testing "a reload whose slice REACHES a quarantined addon is refused too"
        (let [again (hot/reload-addon! host specs "probe.a" {:reload-ns! (fn [_] {:loaded []})})]
          (is (true? (:hot/refused? again)))
          (is (= ["probe.a"] (get (:hot/quarantined again) "probe.a")))))
      (testing "an addon outside every quarantined slice still reloads"
        (let [ok (hot/reload-addon! host specs "probe.b" {:reload-ns! (fn [_] {:loaded []})})]
          (is (:ok? ok) (pr-str (:errors ok)))
          (is (= 2 (generation host "probe.b"))))))))

(deftest a-constructor-that-throws-is-restored-not-quarantined
  (testing "the code loaded; only the new instance is bad. The image is whole,
            the previous instance is restored, and nothing is quarantined"
    (let [host   (mount/atom-mount-host)
          _      (mount/mount! (mount/solve [spec-a]) host)
          report (hot/reload-addon! host [(spec "probe.a" fixture-ns "make-broken")] "probe.a"
                                    {:reload-ns! (fn [_] {:loaded []})})]
      (is (false? (:ok? report)))
      (is (= ["probe.a"] (:hot/restored report)))
      (is (nil? (:hot/reload-failures report)))
      (is (empty? (hot/reload-failures))))))

(deftest the-failure-marks-are-pure-data
  (let [marks  {"x" {:reload/phase :ns-load :reload/error "e" :reload/attempts 1}}
        report {:hot/trigger :manual
                :hot/reload-failures {"y" {:reload/phase :preflight :reload/error "f"}}
                :mounted [{:addon/id "x" :success? true}]}
        after  (failures/next-marks marks report 42)]
    (testing "a successful mount lifts a mark, a failure adds one"
      (is (= ["y"] (keys after)))
      (is (= {:reload/phase :preflight :reload/error "f" :reload/status :reload/failed
              :reload/at 42 :reload/attempts 1 :reload/trigger :manual}
             (get after "y"))))
    (testing "blocked names, per seed, the marked addons its closure reaches"
      (is (= {"a" ["c"]}
             (failures/blocked #{"a" "b"} {"a" #{"a" "c"} "b" #{"b"}} {"c" {}}))))
    (is (str/includes? (failures/refusal-line "a" ["c"] {"c" {:reload/phase :mount :reload/error "boom"}})
                       "c (mount: boom)"))))
