(ns hive-addon.lifecycle
  "Addon lifecycle boundary: mount on first use, release after idle, keep what is
   pinned, and release idle parts of what stays.

     (def mgr (lifecycle/manager {:host h :specs specs
                                  :defaults {:policy :eager}
                                  :overrides {\"hive.carto\" {:policy :lazy :idle-ms 900000}}
                                  :surface-store (store/edn-dir-store dir)}))
     (lifecycle/boot! mgr)          ; mounts eager, stubs dormant
     (lifecycle/install! mgr)       ; dormancy oracle + the installed manager
     (lifecycle/start-sweeper! mgr)

   A stub call runs `activate!`; a real handler runs inside `with-use`, which
   counts it in flight and touches the addon. `sweep!` evicts what the pure plan
   (hive-addon.lifecycle.policy/sweep-plan) allows and closes idle parts.

   Activations and evictions are serialized by one reentrant lock. An eviction
   moves :active to :evicting only while nothing is in flight, and a use only
   starts on an :active addon, so no call runs on an addon being shut down."
  (:require [clojure.string :as str]
            [hive-addon.hot :as hot]
            [hive-addon.mount.solve :as solve]
            [hive-addon.protocol :as proto]
            [hive-addon.hot.source :as source]
            [hive-addon.lifecycle.oracle :as oracle]
            [hive-addon.lifecycle.part :as part]
            [hive-addon.lifecycle.policy :as policy]
            [hive-addon.lifecycle.port :as lport]
            [hive-addon.lifecycle.surface :as surface]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.port :as mport]
            [hive-addon.wire :as wire]
            [hive-dsl.result :as r])
  (:import [java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit]
           [java.util.concurrent.locks ReentrantLock]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def default-lock-timeout-ms 120000)
(def default-sweep-interval-ms 60000)

;; =============================================================================
;; Manager value
;; =============================================================================

(defrecord Manager [host specs states surfaces lock sweeper opts])

(defn- system-now [] (System/currentTimeMillis))

(defn manager
  "A lifecycle manager. OPTS:
     :host           ILifecycleHost (required)
     :specs          [MountSpec ...] every addon this manager governs
     :defaults       LifecycleDecl applied under each manifest's own
     :overrides      {addon-id LifecycleDecl}, strongest
     :surface-store  ISurfaceStore for learned surfaces
     :now-ms         (fn [] ms)
     :reload-ns!     (fn [roots ns-strs] -> reloader answer) run before an
                     activation mounts; default uses hive-hot when present
     :on-activated   (fn [ids]) after addons become active
     :on-evicted     (fn [id]) after an addon becomes dormant
     :lock-timeout-ms"
  [{:keys [host specs] :as opts}]
  {:pre [(satisfies? lport/ILifecycleHost host)]}
  (->Manager host
             (atom (vec specs))
             (atom {})
             (atom {})
             (ReentrantLock.)
             (atom nil)
             (merge {:now-ms system-now
                     :surface-store (lport/->NullSurfaceStore)
                     :lock-timeout-ms default-lock-timeout-ms}
                    (dissoc opts :host :specs))))

(defn- now [mgr] ((get-in mgr [:opts :now-ms])))

(defn- spec-of [mgr id] (some #(when (= id (:addon/id %)) %) @(:specs mgr)))

(defn state [mgr id] (get @(:states mgr) id))

(defn phase [mgr id] (:phase (state mgr id)))

(defn active? [mgr id] (= :active (phase mgr id)))

(defn dormant?
  "True for an addon this manager governs that is not mounted."
  [mgr id]
  (contains? #{:dormant :failed} (phase mgr id)))

(defn- lifecycles [mgr]
  (policy/resolve-all @(:specs mgr) (select-keys (:opts mgr) [:defaults :overrides])))

(defn- with-lock
  "Run F holding the manager lock, or answer (r/err :lifecycle/lock-timeout)."
  [mgr f]
  (let [^ReentrantLock lock (:lock mgr)]
    (if (.tryLock lock (long (get-in mgr [:opts :lock-timeout-ms])) TimeUnit/MILLISECONDS)
      (try (r/ok (f))
           (finally (.unlock lock)))
      (r/err :lifecycle/lock-timeout {:message "lifecycle lock not acquired in time"}))))

(defn- update-state! [mgr id f & args]
  (apply swap! (:states mgr) update id f args))

(defn- fresh-state [id lc]
  {:addon/id id :phase :dormant :lifecycle lc :last-used-ms nil
   :in-flight 0 :activations 0 :evictions 0})

;; =============================================================================
;; Surfaces
;; =============================================================================

(defn surface-of
  "[surface source] for ID: declared, else learned, else [nil :none]."
  [mgr id]
  (if-let [cached (get @(:surfaces mgr) id)]
    cached
    (let [spec    (spec-of mgr id)
          learned (r/rescue nil (lport/-load-surface (get-in mgr [:opts :surface-store]) id))
          res     (surface/resolve-surface spec learned)]
      (swap! (:surfaces mgr) assoc id res)
      res)))

(defn- learn-surface!
  "Record what mounted ID shows, unless its surface is declared. An addon that
   was mounted eagerly only because it had no surface gets its resolved policy
   back once one is learned."
  [mgr id]
  (when-not (surface/declared (spec-of mgr id))
    (let [seen (r/rescue nil (lport/-observe-surface (:host mgr) id))]
      (when-not (surface/blank? seen)
        (r/rescue nil (lport/-save-surface! (get-in mgr [:opts :surface-store]) id seen))
        (swap! (:surfaces mgr) assoc id [(surface/normalize seen) :learned])
        (update-state! mgr id assoc :surface/source :learned)
        (when (= :no-surface (:downgraded (state mgr id)))
          (update-state! mgr id #(-> %
                                     (assoc :lifecycle (get (lifecycles mgr) id))
                                     (dissoc :downgraded))))))))

;; =============================================================================
;; Activation
;; =============================================================================

(declare activate!)

(defn- install-stubs! [mgr id]
  (let [[s src] (surface-of mgr id)]
    (update-state! mgr id assoc :surface/source src)
    (when s
      (r/rescue nil (lport/-install-stubs! (:host mgr) id s #(activate! mgr id))))))

(defn- default-reload-ns!
  [roots ns-strs]
  (when (hot/available?)
    ((hot/ns-reloader roots) ns-strs)))

(defn- reload-before-mount
  "Bring IDS' constructor namespaces up to date. nil when nothing had to run,
   else the reloader's answer."
  [mgr specs]
  (let [reload! (get-in mgr [:opts :reload-ns!] default-reload-ns!)
        roots   (vec (source/watchable-dirs specs))
        ns-strs (vec (distinct (keep :addon/init-ns specs)))]
    (when (seq roots)
      (reload! roots ns-strs))))

(defn- activation-report [id ok? activated & kvs]
  (apply assoc {:addon/id id :ok? ok? :activated (vec activated)} kvs))

(defn- do-activate! [mgr id]
  (let [specs  @(:specs mgr)
        ids    (policy/activation-ids specs id #(active? mgr %))
        wanted (set ids)
        batch  (filterv #(contains? wanted (:addon/id %)) (:ordered (solve/solve specs)))]
    (doseq [i ids] (update-state! mgr i assoc :phase :activating))
    (let [ns-res (r/try-effect (reload-before-mount mgr batch))
          failed (or (r/err? ns-res) (some-> (:ok ns-res) :failed))]
      (if failed
        (let [msg (str "namespace reload before activation failed: "
                       (if (r/err? ns-res) (:message ns-res) (:failed (:ok ns-res))))]
          (doseq [i ids] (update-state! mgr i assoc :phase :dormant :last-error msg))
          (activation-report id false [] :errors [msg]))
        (let [report (r/try-effect (lport/-mount! (:host mgr) batch specs))
              report (if (r/err? report)
                       {:mounted (mapv (fn [i] {:addon/id i :success? false :phase :failed
                                                :errors [(:message report)]})
                                       ids)
                        :ok? false}
                       (:ok report))
              t      (now mgr)
              up     (into [] (comp (filter :success?) (map :addon/id)) (:mounted report))]
          (doseq [{rid :addon/id ok :success? errs :errors} (:mounted report)]
            (if ok
              (do (update-state! mgr rid #(-> %
                                              (assoc :phase :active :last-used-ms t :last-error nil)
                                              (update :activations inc)))
                  (r/rescue nil (lport/-remove-stubs! (:host mgr) rid))
                  (learn-surface! mgr rid))
              (update-state! mgr rid assoc :phase :failed
                             :last-error (str/join "; " (or errs ["mount failed"])))))
          (when-let [f (and (seq up) (get-in mgr [:opts :on-activated]))]
            (r/rescue nil (f up)))
          (cond-> (activation-report id (active? mgr id) up :mounted (vec (:mounted report)))
            (some-> (:ok ns-res) :loaded seq) (assoc :ns-reloaded (mapv str (:loaded (:ok ns-res))))
            (not (active? mgr id))            (assoc :errors [(or (:last-error (state mgr id))
                                                                  "activation failed")])))))))

(defn activate!
  "Mount ID and every dependency it lacks. Idempotent: an active addon answers
   :already-active? true. Returns an ActivationReport."
  [mgr id]
  (cond
    (nil? (spec-of mgr id))
    (activation-report id false [] :errors [(str "no governed spec with :addon/id " (pr-str id))])

    (active? mgr id)
    (activation-report id true [] :already-active? true)

    :else
    (let [res (with-lock mgr #(if (active? mgr id)
                                (activation-report id true [] :already-active? true)
                                (do-activate! mgr id)))]
      (if (r/err? res)
        (activation-report id false [] :errors [(:message res)])
        (:ok res)))))

;; =============================================================================
;; Use tracking
;; =============================================================================

(defn- begin-use!
  "Count one use of ID when it is :active. True when counted."
  [mgr id]
  (let [[old new] (swap-vals! (:states mgr)
                              (fn [m] (if (= :active (get-in m [id :phase]))
                                        (update-in m [id :in-flight] inc)
                                        m)))]
    (not= (get-in old [id :in-flight]) (get-in new [id :in-flight]))))

(defn- end-use! [mgr id]
  (let [t (now mgr)]
    (update-state! mgr id #(-> % (update :in-flight (fn [n] (max 0 (dec n))))
                               (assoc :last-used-ms t)))))

(defn touch!
  "Mark ID used now."
  [mgr id]
  (when (state mgr id)
    (update-state! mgr id assoc :last-used-ms (now mgr)))
  nil)

(defn call-with-use
  "Call F with ID counted in flight. An ungoverned ID runs F untracked. A dormant
   ID is activated first; when that fails F is not called and the
   ActivationReport is thrown as ex-info."
  [mgr id f]
  (cond
    (nil? (state mgr id)) (f)
    (begin-use! mgr id)   (try (f) (finally (end-use! mgr id)))
    :else
    (let [rep (activate! mgr id)]
      (if (and (:ok? rep) (begin-use! mgr id))
        (try (f) (finally (end-use! mgr id)))
        (throw (ex-info (str "addon " id " could not be activated")
                        {:lifecycle/activation rep}))))))

(defmacro with-use
  "Run BODY with addon ID counted in flight (see call-with-use)."
  [mgr id & body]
  `(call-with-use ~mgr ~id (fn [] ~@body)))

(defn tracked
  "F wrapped so every call counts as a use of ID."
  [mgr id f]
  (fn [& args] (call-with-use mgr id #(apply f args))))

;; =============================================================================
;; Eviction
;; =============================================================================

(defn- eviction-report [id evicted? & kvs]
  (apply assoc {:addon/id id :ok? true :evicted? evicted? :teardown/data-preserved? true} kvs))

(defn- claim-eviction!
  "Move ID :active -> :evicting when nothing is in flight. True when claimed."
  [mgr id]
  (let [[old new] (swap-vals! (:states mgr)
                              (fn [m] (let [st (get m id)]
                                        (if (and (= :active (:phase st)) (zero? (:in-flight st)))
                                          (assoc-in m [id :phase] :evicting)
                                          m))))]
    (and (= :active (get-in old [id :phase])) (= :evicting (get-in new [id :phase])))))

(defn- active-dependents [mgr id]
  (into [] (comp (remove #{id}) (filter #(active? mgr %)))
        (policy/dependent-closure @(:specs mgr) #{id})))

(defn- do-evict! [mgr id force?]
  (let [st (state mgr id)
        lc (:lifecycle st)]
    (cond
      (nil? st)                        (eviction-report id false :ok? false :reason :unknown
                                                        :errors [(str "no governed addon " (pr-str id))])
      (not= :active (:phase st))       (eviction-report id false :reason :not-active)
      (and (not force?) (= :pinned (:policy lc))) (eviction-report id false :reason :pinned)
      (seq (active-dependents mgr id)) (eviction-report id false :reason :dependent-active)
      (not (claim-eviction! mgr id))   (eviction-report id false :reason :in-flight)
      :else
      (do
        (learn-surface! mgr id)
        (let [closed (part/close-owner! id)
              res    (r/try-effect (lport/-unmount! (:host mgr) id))
              errs   (cond (r/err? res) [(:message res)]
                           (:ok? (:ok res)) nil
                           :else (vec (:errors (:ok res) ["unmount failed"])))]
          (update-state! mgr id #(-> % (assoc :phase :dormant :in-flight 0)
                                     (update :evictions inc)
                                     (assoc :last-error (first errs))))
          (install-stubs! mgr id)
          (when-let [f (get-in mgr [:opts :on-evicted])] (r/rescue nil (f id)))
          (cond-> (eviction-report id true :parts-closed (vec closed))
            (seq errs) (assoc :errors errs)))))))

(defn evict!
  "Release ID: shut it down, withdraw its surface, advertise stubs. Refuses while
   it is in flight or an active addon depends on it. OPTS {:force? true} also
   evicts a pinned or eager addon. Returns an EvictionReport."
  ([mgr id] (evict! mgr id {}))
  ([mgr id {:keys [force?]}]
   (let [res (with-lock mgr #(do-evict! mgr id (boolean force?)))]
     (if (r/err? res)
       (eviction-report id false :ok? false :reason :lock-timeout :errors [(:message res)])
       (:ok res)))))

;; =============================================================================
;; Sweep
;; =============================================================================

(defn sweep-plan [mgr]
  (policy/sweep-plan (now mgr) @(:specs mgr) @(:states mgr)))

(defn sweep!
  "Evict every addon the sweep plan allows, then close idle parts.
   Returns {:plan SweepPlan :evicted [id ...] :refused {id reason} :parts {owner [part-id]}}."
  [mgr]
  (let [plan    (sweep-plan mgr)
        reports (mapv #(evict! mgr % {}) (:evict plan))]
    {:plan    plan
     :evicted (into [] (comp (filter :evicted?) (map :addon/id)) reports)
     :refused (into {} (comp (remove :evicted?) (map (juxt :addon/id :reason))) reports)
     :parts   (part/sweep! (now mgr))}))

(defn start-sweeper!
  "Sweep every :sweep-interval-ms on a daemon thread. Idempotent."
  ([mgr] (start-sweeper! mgr {}))
  ([mgr {:keys [interval-ms]}]
   (or @(:sweeper mgr)
       (let [ms   (long (or interval-ms default-sweep-interval-ms))
             exec (Executors/newSingleThreadScheduledExecutor
                   (reify ThreadFactory
                     (newThread [_ runnable]
                       (doto (Thread. ^Runnable runnable "hive-addon-lifecycle-sweeper")
                         (.setDaemon true)))))]
         (.scheduleWithFixedDelay ^ScheduledExecutorService exec
                                  ^Runnable (fn [] (r/rescue nil (sweep! mgr)))
                                  ms ms TimeUnit/MILLISECONDS)
         (reset! (:sweeper mgr) {:executor exec :interval-ms ms})
         @(:sweeper mgr)))))

(defn stop-sweeper! [mgr]
  (when-let [{:keys [executor]} @(:sweeper mgr)]
    (.shutdownNow ^ScheduledExecutorService executor)
    (reset! (:sweeper mgr) nil))
  nil)

;; =============================================================================
;; Boot and adoption
;; =============================================================================

(defn boot!
  "Mount what the policy says mounts at boot and stub the rest.
   OPTS {:mount-eager? false} records eager addons as active without mounting
   them, for a host that mounted them itself. Returns a BootReport."
  ([mgr] (boot! mgr {}))
  ([mgr {:keys [mount-eager?] :or {mount-eager? true}}]
   (let [specs (vec @(:specs mgr))
         lcs   (lifecycles mgr)
         part* (policy/boot-partition specs lcs #(first (surface-of mgr %)))
         eager (set (:eager part*))
         t     (now mgr)]
     (doseq [spec specs
             :let [id (:addon/id spec)
                   lc (cond-> (get lcs id)
                        (contains? (:downgraded part*) id) (assoc :policy :eager))]]
       (swap! (:states mgr) assoc id
              (cond-> (fresh-state id lc)
                (contains? (:downgraded part*) id) (assoc :downgraded (get (:downgraded part*) id)))))
     (let [by-id  (into {} (map (juxt :addon/id identity)) specs)
           report (when (and mount-eager? (seq eager))
                    (lport/-mount! (:host mgr) (mapv by-id (:eager part*)) specs))
           up     (if report
                    (into #{} (comp (filter :success?) (map :addon/id)) (:mounted report))
                    eager)]
       (doseq [id eager]
         (if (contains? up id)
           (do (update-state! mgr id assoc :phase :active :last-used-ms t)
               (when report (learn-surface! mgr id)))
           (update-state! mgr id assoc :phase :failed
                          :last-error (some #(when (= id (:addon/id %))
                                               (str/join "; " (:errors % ["mount failed"])))
                                            (:mounted report)))))
       (doseq [id (:dormant part*)] (install-stubs! mgr id))
       (cond-> {:eager      (:eager part*)
                :dormant    (:dormant part*)
                :downgraded (:downgraded part*)
                :ok?        (or (nil? report) (boolean (:ok? report)))}
         report (assoc :mount report))))))

(defn adopt!
  "Govern SPECS that are ALREADY mounted (hot inject, a host's own boot) as active
   addons under the resolved policy. Returns the adopted ids."
  [mgr specs]
  (let [known (set (map :addon/id @(:specs mgr)))]
    (swap! (:specs mgr) into (remove #(contains? known (:addon/id %)) specs)))
  (let [lcs (lifecycles mgr)
        t   (now mgr)]
    (into [] (map (fn [{:keys [addon/id]}]
                    (swap! (:states mgr) update id
                           (fn [st] (assoc (or st (fresh-state id (get lcs id)))
                                           :phase :active :last-used-ms t)))
                    id))
          specs)))

(defn set-policy!
  "Change ID's lifecycle at runtime (e.g. pin it). Returns the new Lifecycle."
  [mgr id decl]
  (-> (swap! (:states mgr) update-in [id :lifecycle] merge decl)
      (get-in [id :lifecycle])))

;; =============================================================================
;; Status and installation
;; =============================================================================

(defn status
  "The manager as wire-safe data."
  [mgr]
  (let [t (now mgr)]
    (wire/json-safe
     {:now-ms  t
      :sweeper (some-> @(:sweeper mgr) (select-keys [:interval-ms]))
      :addons  (into (sorted-map)
                     (map (fn [[id st]]
                            [id (cond-> (select-keys st [:phase :lifecycle :last-used-ms :in-flight
                                                         :activations :evictions :surface/source
                                                         :last-error :downgraded])
                                  (:last-used-ms st) (assoc :idle-for-ms (- t (:last-used-ms st))))]))
                     @(:states mgr))
      :parts   (part/statuses)})))

(defonce ^:private installed (atom nil))

(defn install!
  "Make MGR the installed manager and the source of the dormancy oracle."
  [mgr]
  (reset! installed mgr)
  (oracle/install-dormancy! #(dormant? mgr %))
  mgr)

(defn uninstall! []
  (when-let [mgr @installed] (stop-sweeper! mgr))
  (reset! installed nil)
  (oracle/reset-dormancy!)
  nil)

(defn installed-manager [] @installed)

;; =============================================================================
;; A host over IMountHost, for tests and hosts without a tool surface
;; =============================================================================

(defrecord MountHost [mount-host mount-opts stubs]
  lport/ILifecycleHost
  (-mount! [_ specs peers]
    (boundary/mount! {:ordered specs} mount-host (assoc mount-opts :peer-specs peers)))
  (-unmount! [_ id]
    (let [td (boundary/teardown! mount-host [id])]
      {:ok? (empty? (:errors td)) :errors (vec (:errors td))}))
  (-install-stubs! [_ id s activate!]
    (swap! stubs assoc id {:surface s :activate! activate!})
    nil)
  (-remove-stubs! [_ id]
    (swap! stubs dissoc id)
    nil)
  (-observe-surface [_ id]
    (when-let [addon (mport/registered mount-host id)]
      (surface/observed id (r/rescue [] (vec (proto/tools addon))) {}))))

(defn mount-host
  "An ILifecycleHost over an IMountHost. Stubs are kept in an atom
   ({id {:surface s :activate! f}}) readable as (:stubs host)."
  ([mh] (mount-host mh {}))
  ([mh mount-opts] (->MountHost mh mount-opts (atom {}))))
