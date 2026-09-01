(ns hive-addon.hot.inject-test
  "Injecting an addon that was not on the classpath at boot, and the scoped
   namespace reload against a real hive-hot.

   Both need a live classpath to extend, so every test here runs with a
   DynamicClassLoader installed as the thread's context loader — which is
   what an nREPL-hosted image already has and a cold test runner does not."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.hot :as hot]
            [hive-addon.hot.inject :as inject]
            [hive-addon.hot.schema :as hs]
            [hive-addon.hot.source :as source]
            [hive-addon.hot-fixture :as fx]
            [hive-addon.mount :as mount]
            [hive-addon.mount.port :as port])
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
(def chain-specs [spec-a spec-b])

(defn- with-dynamic-loader [f]
  (let [t   (Thread/currentThread)
        old (.getContextClassLoader t)]
    (.setContextClassLoader t (DynamicClassLoader. old))
    (try (f) (finally (.setContextClassLoader t old)))))

(defn- reset-all! [f]
  (fx/reset-fixture!)
  (with-dynamic-loader f))

(use-fixtures :each reset-all!)

(defn- delete-tree! [^File f]
  (when (.isDirectory f) (run! delete-tree! (.listFiles f)))
  (.delete f))

(defn- unload-ns!
  "Forget `sym` completely: remove the namespace AND drop it from
   *loaded-libs*, or the next `require` is a no-op and the constructor var
   reads as absent."
  [sym]
  (remove-ns sym)
  (dosync (alter @#'clojure.core/*loaded-libs* disj sym)))

(defn- tmp-dir [prefix]
  (doto (io/file (System/getProperty "java.io.tmpdir") (str prefix (System/nanoTime)))
    .mkdirs))

(defn- fresh-project!
  "A throwaway addon project on disk: a deps.edn naming its :paths, a manifest
   under resources/META-INF/hive-addons, and a constructor namespace under
   src that builds a probe addon named `id`."
  [id ns-str deps]
  (let [root (tmp-dir "hive-addon-inject-")
        src  (io/file root "src" (str (source/ns->base-path ns-str) ".clj"))
        man  (io/file root "resources" "META-INF" "hive-addons" (str id ".edn"))]
    ;; A previous run in a warm image may have left the namespace behind.
    (unload-ns! (symbol ns-str))
    (.mkdirs (.getParentFile src))
    (.mkdirs (.getParentFile man))
    (spit (io/file root "deps.edn") (pr-str {:paths ["src" "resources"]}))
    (spit man (pr-str {:addon/id id
                       :addon/type :native
                       :addon/init-ns ns-str
                       :addon/init-fn "make"
                       :addon/dependencies deps}))
    (spit src (str "(ns " ns-str " (:require [hive-addon.hot-fixture :as fx]))\n"
                   "(defn make [config] (fx/make-shared (assoc config :probe/id \"" id "\")))\n"))
    root))

(defn- mount-chain! []
  (let [host (mount/atom-mount-host)]
    (mount/mount! (mount/solve chain-specs) host)
    host))

;; =============================================================================
;; inject!
;; =============================================================================

(deftest injecting-a-project-mounts-its-addon-next-to-the-running-ones
  (let [root (fresh-project! "probe.injected" "probe.injected-one" #{"probe.a"})]
    (try
      (let [host   (mount-chain!)
            report (inject/inject! host chain-specs (str root) {:hot? false})]
        (is (:ok? report) (pr-str (:errors report)))
        (is (= ["probe.injected"] (:hot/discovered report)))
        (is (= ["probe.injected"] (:hot/injected report)))
        (is (= [] (:hot/already-mounted report)))
        (is (= ["probe.injected"] (:hot/affected report)))
        (is (empty? (:hot/torn-down report)))
        (testing "deps.edn :paths are the classpath entries"
          (is (= 2 (count (:hot/classpath report))))
          (is (every? false? (map :already? (:hot/classpath report)))))
        (testing "it is really registered and initialized with its dependency injected"
          (is (some? (port/registered host "probe.injected")))
          (let [[_ id _ deps] (last (fx/events-of :init))]
            (is (= "probe.injected" id))
            (is (= #{"probe.a"} deps))))
        (is (nil? (hs/humanize-errors hs/InjectReport report))
            (pr-str (hs/humanize-errors hs/InjectReport report))))
      (finally
        (unload-ns! 'probe.injected-one)
        (delete-tree! root)))))

(deftest injecting-twice-is-idempotent-and-says-so
  (let [root (fresh-project! "probe.injected" "probe.injected-two" #{})]
    (try
      (let [host (mount-chain!)
            r1   (inject/inject! host chain-specs (str root) {:hot? false})
            r2   (inject/inject! host chain-specs (str root) {:hot? false})]
        (is (:ok? r1) (pr-str (:errors r1)))
        (is (:ok? r2) (pr-str (:errors r2)))
        (is (= ["probe.injected"] (:hot/already-mounted r2)))
        (is (= [] (:hot/injected r2)))
        (is (every? :already? (:hot/classpath r2)))
        (testing "the running instance was left alone"
          (is (= 1 (:generation (port/registered host "probe.injected"))))))
      (finally
        (unload-ns! 'probe.injected-two)
        (delete-tree! root)))))

(deftest a-mounted-dependent-of-the-injected-addon-is-remounted-to-receive-it
  (testing "probe.x depended on an addon that was not there; injecting it
            tears probe.x down and rebuilds it with the new sibling injected"
    (let [root (fresh-project! "probe.injected" "probe.injected-three" #{})
          x    (spec "probe.x" "make-shared"
                     :addon/dependencies #{"probe.injected"}
                     :addon/config {:probe/id "probe.x"})
          specs (conj chain-specs x)]
      (try
        (let [host (mount/atom-mount-host)]
          (mount/mount! (mount/solve specs) host)
          (is (= 1 (:generation (port/registered host "probe.x"))))
          (let [report (inject/inject! host specs (str root) {:hot? false})]
            (is (:ok? report) (pr-str (:errors report)))
            (is (= ["probe.injected" "probe.x"] (:hot/affected report)))
            (is (= ["probe.x"] (:hot/torn-down report)))
            (is (= 2 (:generation (port/registered host "probe.x"))))
            (let [[_ _ _ deps] (last (filter (fn [[_ id _ _]] (= "probe.x" id))
                                             (fx/events-of :init)))]
              (is (= #{"probe.injected"} deps)))))
        (finally
          (unload-ns! 'probe.injected-three)
          (delete-tree! root))))))

(deftest without-a-dynamic-classloader-injection-refuses-instead-of-mounting-blind
  (let [root   (fresh-project! "probe.injected" "probe.injected-four" #{})
        result (promise)
        t      (doto (Thread. (fn [] (deliver result
                                              (inject/inject! (mount/atom-mount-host) []
                                                              (str root) {:hot? false}))))
                 (.setContextClassLoader (ClassLoader/getSystemClassLoader)))]
    (try
      (.start t)
      (.join t 10000)
      (let [report (deref result 1000 ::timeout)]
        (is (map? report))
        (is (false? (:ok? report)))
        (is (some #(re-find #"DynamicClassLoader" %) (:errors report)))
        (is (empty? (:hot/injected report))))
      (finally
        (delete-tree! root)))))

(deftest a-missing-path-is-reported-not-thrown
  (let [report (inject/inject! (mount/atom-mount-host) [] "/no/such/addon/project" {:hot? false})]
    (is (false? (:ok? report)))
    (is (some #(re-find #"does not exist" %) (:errors report)))))

;; =============================================================================
;; The default reloader, against a real hive-hot: scoped to the seeds' roots
;; =============================================================================

(deftest the-default-reloader-declines-another-roots-change
  (when (hot/available?)
    (let [hh-init!  (requiring-resolve 'hive-hot.core/init!)
          hh-reset! (requiring-resolve 'hive-hot.core/reset-all!)
          tmp       (tmp-dir "hive-addon-scoped-")
          other     (io/file tmp "other_session" "edit.clj")
          src       (fn [v] (str "(ns other-session.edit)\n(def value " v ")\n"))]
      (try
        (.mkdirs (.getParentFile other))
        (unload-ns! 'other-session.edit)
        (spit other (src 1))
        (is (not (hive-dsl.result/err? (inject/extend-classpath! (str tmp)))))
        (require 'other-session.edit)
        (hh-init! {:dirs [(:hot/source-dir (source/resolve-source fixture-ns)) (str tmp)]})
        (Thread/sleep 20)
        (spit other (src 2))
        (Thread/sleep 20)
        (let [host   (mount-chain!)
              report (hot/reload-addon! host chain-specs "probe.a")]
          (is (:ok? report) (pr-str (:errors report)))
          (testing "the other root's change is declined and NAMED"
            (is (= ["other-session.edit"] (:hot/ns-skipped report)))
            (is (= 1 @(resolve 'other-session.edit/value))))
          (testing "nothing under the seed's own root had changed"
            (is (true? (:hot/ns-unchanged? report))))
          (testing "and the addon was still remounted, from the code it had"
            (is (= 2 (:generation (port/registered host "probe.a"))))))
        (finally
          (hh-reset!)
          (unload-ns! 'other-session.edit)
          (delete-tree! tmp))))))
