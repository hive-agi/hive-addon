(ns hive-addon.hot.force-test
  "A forced reload repairs an addon whose namespaces diverged in memory from
   its mounted source, and rebuilds the defs that captured them.

   The addon lives in a throwaway project: probe.forced-verbs holds a verb,
   probe.forced-addon (the constructor ns) captures it in a top-level def."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.hot :as hot]
            [hive-addon.hot.inject :as inject]
            [hive-addon.hot-fixture :as fx]
            [hive-addon.mount :as mount]
            [hive-dsl.result :as r])
  (:import [clojure.lang DynamicClassLoader]
           [java.io File]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- with-dynamic-loader [f]
  (let [t   (Thread/currentThread)
        old (.getContextClassLoader t)]
    (.setContextClassLoader t (DynamicClassLoader. old))
    (try (f) (finally (.setContextClassLoader t old)))))

(use-fixtures :each (fn [f] (fx/reset-fixture!) (with-dynamic-loader f)))

(defn- unload-ns! [sym]
  (remove-ns sym)
  (dosync (alter @#'clojure.core/*loaded-libs* disj sym)))

(defn- delete-tree! [^File f]
  (when (.isDirectory f) (run! delete-tree! (.listFiles f)))
  (.delete f))

(defn- verbs-src [v]
  (str "(ns probe.forced-verbs)\n(defn marker [] " v ")\n"))

(def ^:private addon-src
  (str "(ns probe.forced-addon\n"
       "  (:require [hive-addon.hot-fixture :as fx] [probe.forced-verbs :as verbs]))\n"
       "(def handlers {:marker verbs/marker})\n"
       "(defn make [config] (fx/make-shared (assoc config :probe/id \"probe.forced\")))\n"))

(defn- project! []
  (let [root (doto (io/file (System/getProperty "java.io.tmpdir")
                            (str "hive-addon-force-" (System/nanoTime)))
               .mkdirs)
        src  (io/file root "src")
        pdir (doto (io/file src "probe") .mkdirs)]
    (run! unload-ns! '[probe.forced-addon probe.forced-verbs])
    (spit (io/file pdir "forced_verbs.clj") (verbs-src :disk))
    (spit (io/file pdir "forced_addon.clj") addon-src)
    {:root root :src (str src)}))

(defn- diverge!
  "load-file a different probe.forced-verbs from OUTSIDE the tracked root, as a
   load-file from another checkout does. Nothing tracked changes on disk."
  [root]
  (let [f (io/file root "elsewhere.clj")]
    (spit f (verbs-src :diverged))
    (load-file (str f))))

(defn- marker [] ((resolve 'probe.forced-verbs/marker)))
(defn- captured [] ((:marker @(resolve 'probe.forced-addon/handlers))))

(defn- spec [src]
  {:addon/id "probe.forced"
   :addon/type :native
   :addon/init-ns "probe.forced-addon"
   :addon/init-fn "make"
   :hot/source-dirs [src]})

(defn- forced-supported? []
  (some-> (requiring-resolve 'hive-hot.core/scope-plan) meta :arglists
          (->> (some #(= 2 (count %))))))

(deftest force-re-reads-a-diverged-addon-and-its-capturing-defs
  (when (hot/available?)
    (let [hh-init!  (requiring-resolve 'hive-hot.core/init!)
          hh-reset! (requiring-resolve 'hive-hot.core/reset-all!)
          {:keys [root src]} (project!)]
      (try
        (is (not (r/err? (inject/extend-classpath! src))))
        (require 'probe.forced-addon)
        (hh-init! {:dirs [src]})
        (let [specs [(spec src)]
              host  (mount/atom-mount-host)]
          (mount/mount! (mount/solve specs) host)
          (diverge! root)
          (is (= :diverged (marker)))
          (testing "an unforced reload sees an unchanged disk and leaves the image diverged"
            (let [report (hot/reload-addon! host specs "probe.forced")]
              (is (:ok? report) (pr-str (:errors report)))
              (is (true? (:hot/ns-unchanged? report)))
              (is (= :diverged (marker)))))
          (let [report (hot/reload-addon! host specs "probe.forced" {:force? true})]
            (is (:ok? report) (pr-str (:errors report)))
            (if (forced-supported?)
              (testing "force re-reads the root, the verb and the def capturing it"
                (is (false? (:hot/ns-unchanged? report)))
                (is (contains? (set (:hot/ns-reloaded report)) "probe.forced-verbs"))
                (is (contains? (set (:hot/ns-reloaded report)) "probe.forced-addon"))
                (is (= :disk (marker)))
                (is (= :disk (captured))))
              (testing "a hive-hot without :force? says so instead of a silent no-op"
                (is (= :hot/force-unsupported (get-in report [:diagnostic :code])))))))
        (finally
          (hh-reset!)
          (run! unload-ns! '[probe.forced-addon probe.forced-verbs])
          (delete-tree! root))))))

(deftest a-changed-verb-reloads-the-constructor-ns-that-captured-it
  (when (hot/available?)
    (let [hh-init!  (requiring-resolve 'hive-hot.core/init!)
          hh-reset! (requiring-resolve 'hive-hot.core/reset-all!)
          {:keys [root src]} (project!)]
      (try
        (is (not (r/err? (inject/extend-classpath! src))))
        (require 'probe.forced-addon)
        (hh-init! {:dirs [src]})
        (let [specs [(spec src)]
              host  (mount/atom-mount-host)]
          (mount/mount! (mount/solve specs) host)
          (Thread/sleep 20)
          (spit (io/file src "probe" "forced_verbs.clj") (verbs-src :edited))
          (Thread/sleep 20)
          (let [report (hot/reload-addon! host specs "probe.forced")]
            (is (:ok? report) (pr-str (:errors report)))
            (is (contains? (set (:hot/ns-reloaded report)) "probe.forced-addon")
                "the dependent constructor ns is re-evaluated, not only the edited one")
            (is (= :edited (captured)))))
        (finally
          (hh-reset!)
          (run! unload-ns! '[probe.forced-addon probe.forced-verbs])
          (delete-tree! root))))))
