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
            [clojure.string :as str]
            [hive-addon.hot :as hot]
            [hive-addon.hot.cascade :as cascade]
            [hive-addon.hot.dirs :as dirs]
            [hive-addon.hot.mount-driver :as driver]
            [hive-addon.hot.port :as hport]
            [hive-addon.hot.report :as verdict]
            [hive-addon.hot.source :as source]
            [hive-addon.lifecycle :as lc]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.injected :as injected]
            [hive-addon.mount.port :as port]
            [hive-dsl.result :as r]
            [hive-addon.hot.drain :as drain])
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
  "Claim each fresh addon's source dirs in hive-hot, through the IHotDirs port,
   under that addon's id as OWNER — so a later eject! releases exactly its own
   claim and a dir another addon still claims stays watched. Tracks without
   resetting the baseline. {:added [dir] :error string?}"
  [hot-dirs fresh]
  (reduce (fn [acc spec]
            (let [dirs (vec (source/watchable-dirs [spec]))
                  res  (if (seq dirs)
                         (hport/-extend-dirs! hot-dirs {:dirs dirs
                                                        :owner (:addon/id spec)
                                                        :no-reload hot/no-reload})
                         (r/ok {:added []}))]
              (if (r/err? res)
                (cond-> acc (not (:error acc)) (assoc :error (:message res)))
                (update acc :added into (:added (:ok res))))))
          {:added []}
          fresh))

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

(defn- stamp-source-dirs
  "Record the injected project's source DIRECTORIES on a fresh spec, as
   :hot/source-dirs. The URL inject! adds lives on one DynamicClassLoader; a
   reload driven from a thread outside that chain cannot resolve the source
   through the classpath, so the spec has to carry where it lives. Jars are
   left out — their bytes cannot change, so they are no reload root."
  [paths spec]
  (let [dirs (into [] (comp (filter #(.isDirectory (io/file %)))
                            (map #(.getCanonicalPath (io/file %))))
                   paths)]
    (cond-> spec
      (seq dirs) (assoc :hot/source-dirs dirs))))

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

(defn registrable-specs
  "The MountSpecs an inject hands to hive-hot: the ones already mounted
   (`specs`) plus only those `fresh` specs whose id is in `up`, the set of ids
   that mounted. A fresh addon that failed to mount is left out, so hive-hot
   never tracks an addon that is not there.

   Deduplicated by :addon/id. When an id appears more than once (e.g. a
   classpath spec and an injected spec for the same addon), the LAST one wins,
   because a fresh spec carries the :hot/source-dirs the injection stamped.
   Each id keeps the position of its first appearance."
  [specs fresh up]
  (let [candidates (into (vec specs) (filter #(contains? up (:addon/id %))) fresh)
        latest     (into {} (map (juxt :addon/id identity)) candidates)]
    (into []
          (comp (map :addon/id) (distinct) (map latest))
          candidates)))

(defn inject!
  "Mount the addons whose manifests live under `path` into the running `host`,
   alongside `specs` — the MountSpecs already mounted (the reload bridge's
   effective specs), which is what the new ones are solved against and what
   decides which dependents get remounted.

   `path` is a project dir (its deps.edn :paths are the entries), a plain
   source dir, or a jar.

   Each injected spec is stamped with :hot/source-dirs and returned under
   :hot/specs: the host keeps THOSE as the addon's effective specs, so a later
   reload is scoped to the injected source on any thread.

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
          :hot-dirs      IHotDirs override (default: the hive-hot adapter);
                         each fresh addon claims its dirs under its own id
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
            fresh       (mapv #(stamp-source-dirs paths %) fresh)
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
                             (hot/hot! host (registrable-specs specs fresh up)
                                       {:mount-opts mount-opts
                                        :solve-opts (:solve-opts opts {})}))
                dirs-added (if (:hot? opts true)
                             (hot-extend-dirs! (or (:hot-dirs opts) (dirs/hot-dirs)) up-fresh)
                             {:added []})
                errors     (cond-> (vec (:errors td))
                             true (into (comp (remove :success?)
                                              (map (fn [{:keys [addon/id errors phase]}]
                                                     (str id ": " (if (seq errors)
                                                                    (str/join "; " errors)
                                                                    (str "failed at phase " phase))))))
                                        (:mounted report))
                             (:error dirs-added) (conj (str "hive-hot extend-init!: " (:error dirs-added))))]
            (cond-> (assoc base
                           :hot/specs fresh
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
  "Release, through the IHotDirs port, each ejected addon's claim on the RELEASED
   dirs it lives under (one remove-dirs! request per owner), and fold hive-hot's
   RemoveDirsReports with hive-addon.hot.report/dirs-outcome.
   {:removed [..] :retained [..] :shared {dir [owner]} :reason string?}"
  [hot-dirs released dirs-by-id]
  (let [reqs (into [] (keep (fn [[id ds]]
                              (let [ds (filterv (set released) ds)]
                                (when (seq ds) {:dirs ds :owner id}))))
                   dirs-by-id)]
    (if (empty? reqs)
      {:removed [] :retained [] :shared {}}
      (verdict/dirs-outcome reqs (mapv #(hport/-remove-dirs! hot-dirs %) reqs)))))

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

(defn- eject-slice!
  "The teardown half of eject!, run once the calls in flight have drained:
   KNOWN go, BLOCKING dependents come back without them. Returns BASE filled in."
  [host opts base known by-id mounted blocking]
  (let [gone       (set known)
        driver     (or (:mount-driver opts) (driver/mount-driver))
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
        entries-of (into [] (keep injected/entry) known) ; read BEFORE forget!
        dirs-by-id (into [] (map (fn [id]
                                   [id (vec (distinct
                                             (concat (:hot/dirs (some #(when (= id (:addon/id %)) %) entries-of))
                                                     (some->> (by-id id) vector source/watchable-dirs))))]))
                         known)
        released   (verdict/released-dirs (mapcat second dirs-by-id)
                                          (source/watchable-dirs remaining))
        removed    (hot-remove-dirs! (or (:hot-dirs opts) (dirs/hot-dirs)) released dirs-by-id)
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
                   :hot/dirs-retained (:retained removed)
                   :hot/classpath-retained (vec (distinct (mapcat :hot/classpath entries-of)))
                   :hot/namespaces-retained (vec (sort (distinct (keep (comp #(some-> % str) :addon/init-ns)
                                                                       ejected))))
                   :teardown/data-preserved? (verdict/data-preserved? [td])
                   :ok? (and (empty? errors) (or (nil? remount) (:ok? remount))))
      (:reason removed)        (assoc :hot/dirs-reason (:reason removed))
      (seq (:shared removed))  (assoc :hot/dirs-shared (:shared removed))
      remount           (assoc :hot/remounted blocking :mounted (vec (:mounted remount)))
      (seq errors)      (assoc :errors errors))))

(defn eject!
  "Plug the addons TARGET names OUT of the running `host` — the inverse of
   inject!. `specs` are the MountSpecs currently mounted (the same set inject!
   takes); TARGET is an addon id, a collection of ids, or the path they were
   injected from.

   First the calls in flight are DRAINED: the IDrainGate (:drain-gate, default
   hive-addon.hot.drain/drain-gate) stops admitting calls to the ejected addons
   and the dependents a cascade takes down, and eject! waits up to :drain-ms
   (default hive-addon.hot.drain/default-drain-ms) for the admitted ones to
   finish. Calls still running then REFUSE the eject (:hot/busy? true, nothing
   torn down, the gate re-opened) unless {:force? true}, which goes ahead and
   says so (:hot/forced? true). The wait is reported under :hot/drain.

   Then, in order: tear the addons down (reverse dependency order, through the
   IMountDriver), unregister them through the optional IMountUnregister port,
   stop the installed lifecycle manager governing them, deregister them from
   hive-hot, stop watching the source dirs no surviving addon still needs, and
   forget them in the injected-spec registry. The gate is re-opened afterwards.

   An addon other MOUNTED addons depend on is REFUSED (:hot/refused? true,
   :hot/blocking) unless opts {:cascade? true}, which also tears those
   dependents down and remounts them WITHOUT the ejected sibling.

   What cannot be removed is REPORTED, not hidden: classpath URLs stay on the
   DynamicClassLoader (a URLClassLoader cannot drop one), loaded namespaces stay
   in the image, and a host without IMountUnregister keeps an inert entry.

   Source dirs go through the IHotDirs port (:hot-dirs, default the hive-hot
   adapter): each ejected addon releases its OWN claim, and what hive-hot keeps
   (a core dir, or one another owner claims) is reported under
   :hot/dirs-retained / :hot/dirs-shared.

   opts: {:cascade? :force? :drain-ms :poll-ms :drain-gate :mount-driver
   :hot-dirs :mount-opts :solve-opts} (+ the hive-addon.hot/mount-opt-keys
   folded in from the top level).
   Returns an EjectReport. Never throws. See `plug-out!` for the Result form."
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
      (let [gate    (or (:drain-gate opts) (drain/drain-gate))
            closing (into (vec known) blocking)
            dr      (drain/drain! gate closing (select-keys opts [:drain-ms :poll-ms :now :sleep!]))
            v       (drain/drain-verdict dr (:force? opts))]
        (if (= :busy v)
          (do (drain/open! gate closing)
              (assoc base :ok? false :hot/busy? true :hot/drain dr
                     :errors [(drain/busy-message dr)]))
          (try
            (cond-> (assoc (eject-slice! host opts base known by-id mounted blocking)
                           :hot/drain dr)
              (= :forced v) (assoc :hot/forced? true))
            (finally (drain/open! gate closing))))))))

;; =============================================================================
;; plug-out! — eject! on the railway
;; =============================================================================

(defn plug-out!
  "`eject!` as a hive-dsl Result, for callers composing on the railway.

   (r/ok EjectReport) when the addons are out. An UNSAFE or impossible eject is
   refused LOUDLY, as an err Result carrying the whole report:
     :hot/eject-refused  mounted addons depend on the target (see :hot/blocking;
                         nothing was touched — pass {:cascade? true})
     :hot/eject-busy     calls were still in flight when :drain-ms ran out (see
                         :hot/drain; nothing was touched, calls are admitted
                         again: retry, raise :drain-ms, or pass {:force? true})
     :hot/eject-unknown  nothing by that name is mounted or injected
     :hot/eject-failed   the eject ran but a step failed (see :errors)
   Each err has :message. Never throws."
  [host specs target & [opts]]
  (let [report (eject! host specs target opts)]
    (cond
      (:ok? report) (r/ok report)
      (:hot/refused? report)
      (r/err :hot/eject-refused (assoc report :message (first (:errors report))))
      (:hot/busy? report)
      (r/err :hot/eject-busy (assoc report :message (first (:errors report))))
      (empty? (:hot/ejected report))
      (r/err :hot/eject-unknown (assoc report :message (first (:errors report))))
      :else
      (r/err :hot/eject-failed
             (assoc report :message (str/join "; " (:errors report)))))))
