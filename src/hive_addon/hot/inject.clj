(ns hive-addon.hot.inject
  "Plug an addon IN that was NOT on the classpath when the host booted — and
   plug it back OUT.

   `inject!` is the mount pipeline run over a slice that did not exist yet:
   put the addon's paths on the live classpath, discover the manifests under
   them (and ONLY under them — an addon already on the classpath that the
   composer chose not to mount must not be resurrected by a scan), solve the
   new specs against the peers already mounted, tear down the mounted
   dependents that now have a new sibling to receive, mount the slice through
   the ordinary IMountDriver, and register the new addons with hive-hot so
   they reload like the rest. Every injected addon that mounts is REMEMBERED
   in the injected-spec registry (hive-addon.mount.injected), which classpath
   discovery unions in, so it stays discoverable from any thread; and it is
   ADOPTED by the installed lifecycle manager under its manifest policy.

   `eject!` is the inverse: teardown, unregister through the optional
   IMountUnregister port, un-govern, un-hot, stop watching the dirs nobody else
   needs, forget the registry entry — and REPORT what cannot be removed (the
   classpath URLs a URLClassLoader cannot drop, the namespaces still loaded).

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
            [hive-addon.hot.report :as verdict]
            [hive-addon.hot.source :as source]
            [hive-addon.lifecycle :as lc]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.injected :as injected]
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
   ;; Computed over the teardowns that ran — none yet.
   :teardown/data-preserved? (verdict/data-preserved? [])
   :mounted []
   :ok? true})

(defn- remember-injected!
  "Record the injected specs that actually MOUNTED in the injected-spec registry,
   so classpath discovery from any thread keeps seeing them. Returns the ids."
  [mounted-specs path paths cp dirs]
  (injected/remember!
   (mapv (fn [spec]
           {:addon/id (:addon/id spec)
            :spec spec
            :hot/path path
            :hot/paths (vec paths)
            :hot/classpath (mapv :url cp)
            :hot/dirs (vec (source/watchable-dirs [spec]))})
         mounted-specs)))

(defn- adopt-into-lifecycle!
  "Hand the mounted injected specs to the INSTALLED lifecycle manager, read
   through its var at call time, so they are governed under their manifest
   policy like every booted addon. [] when no manager is installed."
  [mounted-specs]
  (if-let [mgr (lc/installed-manager)]
    (vec (r/rescue [] (lc/adopt! mgr mounted-specs)))
    []))

(defn inject!
  "Mount the addons whose manifests live under `path` into the running `host`,
   alongside `specs` — the MountSpecs already mounted (the reload bridge's
   effective specs), which is what the new ones are solved against and what
   decides which dependents get remounted.

   `path` is a project dir (its deps.edn :paths are the entries), a plain
   source dir, or a jar.

   Every injected addon that mounts is REMEMBERED in the injected-spec registry
   (hive-addon.mount.injected), so discovery from any thread keeps seeing it,
   and ADOPTED by the installed lifecycle manager (when one is installed) under
   its manifest policy. `eject!` reverses both.

   opts: {:mount-opts    forwarded to mount! (e.g. :resolve-config); the keys
                         in hive-addon.hot/mount-opt-keys are folded in from
                         the top level as well
          :solve-opts    forwarded to solve (:rules)
          :mount-driver  IMountDriver override
          :loader        DynamicClassLoader override
          :resolve-deps? hand the project's :deps to add-libs first
          :hot?          register the new addons with hive-hot (default true)
          :govern?       adopt into the installed lifecycle manager (default true)}

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
                up         (into #{} (comp (filter :success?) (map :addon/id)) (:mounted report))
                up-fresh   (filterv #(contains? up (:addon/id %)) fresh)
                remembered (remember-injected! up-fresh path paths cp
                                               (source/watchable-dirs up-fresh))
                adopted    (if (:govern? opts true) (adopt-into-lifecycle! up-fresh) [])
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
                           :hot/remembered (vec remembered)
                           :hot/adopted adopted
                           :teardown/data-preserved? (verdict/data-preserved? [td])
                           :ok? (and (:ok? report) (empty? (:errors td))))
              (seq (:missing plan)) (assoc :hot/missing (:missing plan))
              (seq errors)          (assoc :errors errors))))))))

;; =============================================================================
;; eject! — plug OUT: the inverse of inject!
;; =============================================================================

(defn- target-ids
  "The addon ids TARGET names. TARGET is an addon id, a collection of ids, or
   the path something was injected from (every id remembered for that path).
   Pure over the injected ENTRIES."
  [entries target]
  (cond
    (coll? target) (vec (distinct target))
    :else
    (let [canon  (r/rescue (str target) (str (.getCanonicalFile (io/file (str target)))))
          by-path (injected/entries-for-path entries canon)]
      (if (seq by-path)
        (mapv :addon/id by-path)
        [(str target)]))))

(defn- hot-remove-dirs!
  "Hand DIRS to hive-hot's `remove-dirs!` ({:dirs [..]} -> {:removed [..]
   :kept [..] :absent [..] :dirs [..]}), resolved at call time. A dir hive-hot
   KEEPS (one its initial init declared) is retained; a dir it was not tracking
   was never watched and is neither. hive-hot releases without remove-dirs! leave
   every dir watched and the answer says why.
   {:removed [..] :retained [..] :reason string?}"
  [dirs]
  (cond
    (empty? dirs) {:removed [] :retained []}
    :else
    (if-let [remove! (r/rescue nil (requiring-resolve 'hive-hot.core/remove-dirs!))]
      (let [res (r/try-effect (remove! {:dirs (vec dirs)}))
            out (:ok res)]
        (cond
          (r/err? res)
          {:removed [] :retained (vec dirs) :reason (str "hive-hot remove-dirs! failed: " (:message res))}

          (not (map? out))
          {:removed [] :retained (vec dirs) :reason "hive-hot remove-dirs! answered no report"}

          :else
          (cond-> {:removed (vec (:removed out)) :retained (vec (:kept out))}
            (seq (:kept out))
            (assoc :reason "hive-hot keeps the dirs its initial init declared"))))
      {:removed [] :retained (vec dirs)
       :reason (if (hot/available?)
                 "hive-hot has no remove-dirs! — the dirs stay watched until hive-hot is re-initialized"
                 "hive-hot is not on the classpath — nothing was watching")})))

(defn- eject-base
  [target ids]
  {:hot/target target
   :hot/ejected (vec ids)
   :hot/torn-down []
   :hot/unregistered []
   :hot/unsupported []
   :hot/ungoverned []
   :hot/unhot []
   :hot/forgotten []
   :hot/dirs-removed []
   :hot/dirs-retained []
   :hot/classpath-retained []
   :hot/namespaces-retained []
   :teardown/data-preserved? (verdict/data-preserved? [])
   :ok? true})

(defn eject!
  "Plug the addons TARGET names OUT of the running `host` — the inverse of
   inject!. `specs` are the MountSpecs currently mounted (the same set inject!
   takes); TARGET is an addon id, a collection of ids, or the path they were
   injected from.

   In order: tear the addons down (reverse dependency order, through the
   IMountDriver), unregister them through the optional IMountUnregister port,
   stop the installed lifecycle manager governing them, deregister them from
   hive-hot, stop watching the source dirs no surviving addon still needs, and
   forget them in the injected-spec registry.

   An addon other MOUNTED addons depend on is REFUSED (:hot/refused? true,
   :hot/blocking) unless opts {:cascade? true}, which also tears those
   dependents down and remounts them WITHOUT the ejected sibling.

   What cannot be removed is REPORTED, not hidden: classpath URLs stay on the
   DynamicClassLoader (a URLClassLoader cannot drop one), loaded namespaces stay
   in the image, and a host without IMountUnregister keeps an inert entry.

   opts: {:cascade? :mount-driver :mount-opts :solve-opts} (+ the
   hive-addon.hot/mount-opt-keys folded in from the top level).
   Returns an EjectReport. Never throws."
  [host specs target & [opts]]
  (let [opts      (or opts {})
        entries   (injected/entries)
        ids       (target-ids entries target)
        by-id     (into {} (map (juxt :addon/id identity)) (concat (map :spec entries) specs))
        known     (filterv #(or (by-id %) (some? (r/rescue nil (port/registered host %)))) ids)
        unknown   (filterv (complement (set known)) ids)
        base      (cond-> (eject-base target known)
                    (seq unknown) (assoc :hot/unknown unknown))
        gone      (set known)
        mounted   (vec specs)
        closure   (cascade/dependents mounted gone (:solve-opts opts {}))
        blocking  (vec (sort (remove gone closure)))]
    (cond
      (empty? known)
      (assoc base :ok? false :errors [(str "nothing to eject for " (pr-str target))])

      (and (seq blocking) (not (:cascade? opts)))
      (assoc base :ok? false :hot/refused? true :hot/blocking blocking
             :errors [(str "mounted addons depend on " (pr-str (vec (sort known)))
                           ": " (pr-str blocking) " — pass :cascade? true to remount them without it")])

      :else
      (let [driver     (or (:mount-driver opts) (driver/mount-driver))
            ejected    (keep by-id known)
            remaining  (into [] (remove #(contains? gone (:addon/id %))) mounted)
            ;; Tear the dependents AND the ejected down in one reverse-order pass.
            order      (mapv :addon/id (:ordered (cascade/affected-plan
                                                  (into remaining ejected)
                                                  (into gone blocking)
                                                  (:solve-opts opts {}))))
            td         (hport/-teardown! driver host (if (seq order) order known))
            un         (boundary/unregister! host known)
            mgr        (lc/installed-manager)
            ungoverned (if mgr (vec (r/rescue [] (lc/forget! mgr known))) [])
            unhot      (hot/unhot! ejected)
            entries-of (keep injected/entry known)
            dirs       (verdict/released-dirs
                        (into (vec (mapcat :hot/dirs entries-of)) (source/watchable-dirs ejected))
                        (source/watchable-dirs remaining))
            removed    (hot-remove-dirs! dirs)
            forgotten  (injected/forget! known)
            ;; Dependents come back without the ejected sibling.
            remount    (when (seq blocking)
                         (let [plan (cascade/affected-plan remaining (set blocking) (:solve-opts opts {}))
                               mopts (merge (select-keys opts hot/mount-opt-keys)
                                            (:mount-opts opts {})
                                            {:peer-specs remaining})]
                           (hport/-mount! driver plan host mopts)))
            errors     (cond-> (into (vec (:errors td)) (:errors un))
                         remount (into (comp (remove :success?)
                                             (map #(str (:addon/id %) ": remount without "
                                                        (pr-str (vec (sort known))) " failed")))
                                       (:mounted remount)))]
        (cond-> (assoc base
                       :hot/torn-down (vec (:torn-down td))
                       :hot/unregistered (vec (:unregistered un))
                       :hot/unsupported (vec (:unsupported un))
                       :hot/ungoverned ungoverned
                       :hot/unhot (vec (:hot/unregistered unhot))
                       :hot/forgotten forgotten
                       :hot/dirs-removed (:removed removed)
                       :hot/dirs-retained (vec (sort (distinct (:retained removed))))
                       :hot/classpath-retained (vec (distinct (mapcat :hot/classpath entries-of)))
                       :hot/namespaces-retained (vec (sort (distinct (keep (comp #(some-> % str) :addon/init-ns)
                                                                           ejected))))
                       :teardown/data-preserved? (verdict/data-preserved? [td])
                       :ok? (and (empty? errors) (or (nil? remount) (:ok? remount))))
          (:reason removed) (assoc :hot/dirs-reason (:reason removed))
          remount           (assoc :hot/remounted blocking :mounted (vec (:mounted remount)))
          (seq errors)      (assoc :errors errors))))))
