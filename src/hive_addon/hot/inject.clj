(ns hive-addon.hot.inject
  "Inject an addon that was NOT on the classpath when the host booted.

   `inject!` is the mount pipeline run over a slice that did not exist yet:
   put the addon's paths on the live classpath, discover the manifests under
   them (and ONLY under them — an addon already on the classpath that the
   composer chose not to mount must not be resurrected by a scan), solve the
   new specs against the peers already mounted, tear down the mounted
   dependents that now have a new sibling to receive, mount the slice through
   the ordinary IMountDriver, and register the new addons with hive-hot so
   they reload like the rest.

   Classpath extension is a JVM concern: the URL is added to the highest
   DynamicClassLoader above the calling thread's context loader, which is the
   loader every later `require` resolves through in an nREPL-hosted image. A
   thread with no DynamicClassLoader in its chain cannot extend the classpath;
   inject! then REFUSES with :hot/no-dynamic-classloader rather than mounting
   code the next require would fail to find.

   Maven dependencies of the injected addon are NOT resolved by default; pass
   `:resolve-deps? true` to hand the project's deps.edn :deps to
   clojure.repl.deps/add-libs first (needs a tools.deps basis in the image)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hive-addon.hot :as hot]
            [hive-addon.hot.cascade :as cascade]
            [hive-addon.hot.mount-driver :as driver]
            [hive-addon.hot.port :as hport]
            [hive-addon.hot.source :as source]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.port :as port]
            [hive-dsl.result :as r])
  (:import [clojure.lang DynamicClassLoader RT]
           [java.io File]
           [java.net URL URLClassLoader]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Classpath — the JVM seam
;; =============================================================================

(defn- loader-chain
  [^ClassLoader start]
  (take-while some? (iterate #(.getParent ^ClassLoader %) start)))

(defn dynamic-loader
  "The highest DynamicClassLoader in the calling thread's loader chain — the
   context loader's chain first, then clojure.lang.RT/baseLoader's — or nil
   when there is none."
  []
  (letfn [(highest [start]
            (last (filter #(instance? DynamicClassLoader %) (loader-chain start))))]
    (or (highest (.getContextClassLoader (Thread/currentThread)))
        (highest (RT/baseLoader)))))

(defn- path-url
  ^URL [path]
  (.toURL (.toURI (.getCanonicalFile (io/file path)))))

(defn- known-urls
  "Every URL any URLClassLoader in `loader`'s chain already carries."
  [^ClassLoader loader]
  (into #{}
        (comp (filter #(instance? URLClassLoader %))
              (mapcat #(.getURLs ^URLClassLoader %))
              (map str))
        (loader-chain loader)))

(defn extend-classpath!
  "Put `path` (a directory or a jar) on the live classpath. Idempotent: a URL
   the loader chain already carries is reported :already? true and not
   re-added.

   (r/ok {:url .. :already? bool :loader <loader>}) on success;
   (r/err :hot/path-absent ..) when the path does not exist;
   (r/err :hot/no-dynamic-classloader ..) when no loader can be extended."
  ([path] (extend-classpath! path {}))
  ([path {:keys [loader]}]
   (let [f (io/file path)]
     (if-not (.exists f)
       (r/err :hot/path-absent {:path (str path)
                                :message (str "path does not exist: " path)})
       (if-let [^DynamicClassLoader dcl (or loader (dynamic-loader))]
         (let [url      (path-url f)
               already? (contains? (known-urls dcl) (str url))]
           (when-not already? (.addURL dcl url))
           (r/ok {:url (str url) :already? already? :loader (str dcl)}))
         (r/err :hot/no-dynamic-classloader
                {:path (str path)
                 :message (str "no DynamicClassLoader in this thread's loader chain — "
                               "run inject! from a REPL-hosted thread, or pass :loader")}))))))

;; =============================================================================
;; What a path stands for
;; =============================================================================

(defn project-paths
  "The classpath entries `path` stands for. A directory holding a deps.edn
   contributes its :paths (default [\"src\"]) resolved against it; anything
   else — a plain source dir, a jar — is itself the one entry."
  [path]
  (let [f    (io/file path)
        deps (io/file f "deps.edn")]
    (if (and (.isDirectory f) (.exists deps))
      (let [m (r/rescue {} (edn/read-string (slurp deps)))]
        (mapv #(str (io/file f ^String %)) (or (seq (:paths m)) ["src"])))
      [(str f)])))

(defn manifests-under
  "MountSpecs whose manifest lives under `paths` — discovered through a
   throwaway URLClassLoader over exactly those entries and no parent, so
   nothing else on the classpath is seen. Returns {:specs [..] :errors [..]}."
  [paths]
  (let [urls (into-array URL (map path-url paths))]
    (with-open [cl (URLClassLoader. urls nil)]
      (boundary/discover-specs cl))))

(defn- resolve-deps!
  "Hand the project's deps.edn :deps — minus the libs the image must keep
   single, Clojure and hive-addon itself — to clojure.repl.deps/add-libs.
   Never throws. {:ok? bool :added [..]} or {:ok? false :error ..}."
  [path]
  (let [deps (io/file path "deps.edn")]
    (if-not (.exists deps)
      {:ok? true :added [] :skipped? true}
      (let [m    (r/rescue {} (edn/read-string (slurp deps)))
            libs (dissoc (:deps m {}) 'org.clojure/clojure 'io.github.hive-agi/hive-addon)]
        (if-let [add-libs (r/rescue nil (requiring-resolve 'clojure.repl.deps/add-libs))]
          (let [res (r/try-effect (binding [*repl* true] (add-libs libs)))]
            (if (r/err? res)
              {:ok? false :error (:message res)}
              {:ok? true :added (vec (:ok res))}))
          {:ok? false :error "clojure.repl.deps/add-libs is unavailable (Clojure < 1.12)"})))))

;; =============================================================================
;; inject! — the mount pipeline over a slice that did not exist yet
;; =============================================================================

(defn- hot-extend-dirs!
  "Hand `dirs` to hive-hot's `extend-init!` so the new addons are tracked and
   watched without resetting the baseline. Silent without hive-hot."
  [dirs]
  (if-let [extend! (r/rescue nil (requiring-resolve 'hive-hot.core/extend-init!))]
    (let [res (r/try-effect (extend! {:dirs (vec dirs) :no-reload hot/no-reload}))]
      (if (r/err? res)
        {:added [] :error (:message res)}
        (select-keys (:ok res) [:added :dirs])))
    {:added []}))

(defn- base-report
  [path paths]
  {:hot/path path
   :hot/paths (vec paths)
   :hot/classpath []
   :hot/discovered []
   :hot/already-mounted []
   :hot/injected []
   :hot/affected []
   :hot/torn-down []
   :hot/dirs-added []
   :hot/registered []
   :teardown/data-preserved? true
   :mounted []
   :ok? true})

(defn inject!
  "Mount the addons whose manifests live under `path` into the running `host`,
   alongside `specs` — the MountSpecs already mounted (the reload bridge's
   effective specs), which is what the new ones are solved against and what
   decides which dependents get remounted.

   `path` is a project dir (its deps.edn :paths are the entries), a plain
   source dir, or a jar.

   opts: {:mount-opts    forwarded to mount! (e.g. :resolve-config); the keys
                         in hive-addon.hot/mount-opt-keys are folded in from
                         the top level as well
          :solve-opts    forwarded to solve (:rules)
          :mount-driver  IMountDriver override
          :loader        DynamicClassLoader override
          :resolve-deps? hand the project's :deps to add-libs first
          :hot?          register the new addons with hive-hot (default true)}

   Returns an InjectReport. Never throws; every failure is in the report."
  [host specs path & [opts]]
  (let [opts  (or opts {})
        path  (str (.getCanonicalFile (io/file path)))
        paths (project-paths path)
        deps  (when (:resolve-deps? opts) (resolve-deps! path))
        cp    (reduce (fn [acc p]
                        (let [res (extend-classpath! p opts)]
                          (if (r/err? res) (reduced res) (conj acc (:ok res)))))
                      []
                      paths)
        base  (cond-> (base-report path paths)
                deps (assoc :hot/deps deps))]
    (if (r/err? cp)
      (assoc base :ok? false :errors [(:message cp)])
      (let [{new-specs :specs errors :errors} (manifests-under paths)
            mounted     (into #{} (map :addon/id) specs)
            registered? (fn [id] (or (contains? mounted id)
                                     (some? (r/rescue nil (port/registered host id)))))
            {fresh false already true} (group-by (comp boolean registered? :addon/id) new-specs)
            fresh       (vec fresh)
            fresh-ids   (into #{} (map :addon/id) fresh)
            base        (cond-> (assoc base
                                       :hot/classpath (vec cp)
                                       :hot/discovered (mapv :addon/id new-specs)
                                       :hot/already-mounted (mapv :addon/id already)
                                       :hot/injected (vec (sort fresh-ids)))
                          (seq errors) (assoc :discovery-errors (vec errors)))]
        (if (empty? fresh)
          base
          (let [all        (into (vec specs) fresh)
                driver     (or (:mount-driver opts) (driver/mount-driver))
                plan       (cascade/affected-plan all fresh-ids (:solve-opts opts {}))
                ordered    (:ordered plan)
                ids        (mapv :addon/id ordered)
                remounted  (filterv (complement fresh-ids) ids)
                mount-opts (merge (select-keys opts hot/mount-opt-keys)
                                  (:mount-opts opts {}))
                td         (if (seq remounted)
                             (hport/-teardown! driver host remounted)
                             {:torn-down []})
                report     (hport/-mount! driver plan host (assoc mount-opts :peer-specs all))
                hot-report (when (:hot? opts true)
                             (hot/hot! host all {:mount-opts mount-opts
                                                 :solve-opts (:solve-opts opts {})}))
                dirs       (source/watchable-dirs fresh)
                dirs-added (if (and (:hot? opts true) (seq dirs))
                             (hot-extend-dirs! dirs)
                             {:added []})
                errors     (cond-> (vec (:errors td))
                             true (into (comp (remove :success?)
                                              (map (fn [{:keys [addon/id errors phase]}]
                                                     (str id ": " (if (seq errors)
                                                                    (clojure.string/join "; " errors)
                                                                    (str "failed at phase " phase))))))
                                        (:mounted report))
                             (:error dirs-added) (conj (str "hive-hot extend-init!: " (:error dirs-added))))]
            (cond-> (assoc base
                           :hot/affected ids
                           :hot/torn-down (vec (:torn-down td))
                           :mounted (vec (:mounted report))
                           :hot/dirs-added (vec (:added dirs-added))
                           :hot/registered (mapv :addon/id (:hot/registered hot-report))
                           :ok? (and (:ok? report) (empty? (:errors td))))
              (seq (:missing plan)) (assoc :hot/missing (:missing plan))
              (seq errors)          (assoc :errors errors))))))))
