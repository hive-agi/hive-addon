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

;; SEAT is an atom shared by every Manager built over the same state (see
;; reseat-host!); it holds the Manager currently governing that state. Anything
;; that touches the host resolves the manager through it at CALL time, never
;; through a Manager value captured earlier (Capture-by-Var).
(defrecord Manager [host specs states surfaces lock sweeper opts seat])

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
  (let [seat (atom nil)
        mgr  (->Manager host
                        (atom (vec specs))
                        (atom {})
                        (atom {})
                        (ReentrantLock.)
                        (atom nil)
                        (merge {:now-ms system-now
                                :surface-store (lport/->NullSurfaceStore)
                                :lock-timeout-ms default-lock-timeout-ms}
                               (dissoc opts :host :specs))
                        seat)]
    (reset! seat mgr)
    mgr))

(defn current
  "The Manager now governing MGR's state: the one a later reseat-host! seated,
   else MGR itself. Resolved at call time, so a caller holding an older Manager
   value still reaches the current host."
  [mgr]
  (or (some-> (:seat mgr) deref) mgr))

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
  "Run (F seated) holding the manager lock, where SEATED is the manager current
   ONCE THE LOCK IS HELD (see `current`), or answer (r/err :lifecycle/lock-timeout).
   reseat-host! swaps the seat under this same lock, so F never sees a host that
   a completed reseat replaced."
  [mgr f]
  (let [^ReentrantLock lock (:lock mgr)]
    (if (.tryLock lock (long (get-in mgr [:opts :lock-timeout-ms])) TimeUnit/MILLISECONDS)
      (try (r/ok (f (current mgr)))
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

(declare installed-manager)

(defn- stub-manager
  "The manager a stub call activates through, resolved at CALL time: the
   installed manager when it governs MGR's state (same lock), else MGR's current
   seat. A stub armed before a reseat therefore wakes the addon on the new host."
  [mgr]
  (let [inst (installed-manager)]
    (if (and inst (identical? (:lock inst) (:lock mgr)))
      inst
      (current mgr))))

(defn- install-stubs! [mgr id]
  (let [[s src] (surface-of mgr id)]
    (update-state! mgr id assoc :surface/source src)
    (when s
      (r/rescue nil (lport/-install-stubs! (:host mgr) id s #(activate! (stub-manager mgr) id))))))

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
    (let [res (with-lock mgr (fn [m] (if (active? m id)
                                       (activation-report id true [] :already-active? true)
                                       (do-activate! m id))))]
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

(defn- refused-eviction
  "An EvictionReport for an eviction that did not happen. Never dressed as
   success, and it makes no data-preservation claim: nothing was released."
  [id reason & {:as extra}]
  (merge {:addon/id id :ok? false :evicted? false :refused? true :reason reason} extra))

(defn- released-eviction
  "An EvictionReport for an addon that was released. :teardown/data-preserved?
   is carried over from what the host's unmount CLAIMED; a host that makes no
   claim gets no verdict (absent, i.e. unknown) rather than an asserted true."
  [id unmounted & {:as extra}]
  (merge (cond-> {:addon/id id :ok? true :evicted? true}
           (contains? unmounted :teardown/data-preserved?)
           (assoc :teardown/data-preserved? (true? (:teardown/data-preserved? unmounted))))
         extra))

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

(def eviction-guards
  "Why an eviction is REFUSED, as an ordered rule table: [reason refuses?]
   where (refuses? mgr id state force?) answers true to refuse for that reason.
   The first rule that fires is the report's :reason. A new reason is a new
   row; do-evict! never changes.

   :no-surface refuses a surface-less (e.g. hooks-only) addon: evicted, it
   would have no stub to route a call back through, so its hooks and whole
   contribution would vanish and it would never re-mount. :force? overrides it
   exactly as it overrides :pinned — the operator then owns the consequence."
  [[:not-active       (fn [_ _ st _] (not= :active (:phase st)))]
   [:pinned           (fn [_ _ st force?] (and (not force?) (= :pinned (get-in st [:lifecycle :policy]))))]
   [:no-surface       (fn [mgr id _ force?] (and (not force?) (nil? (first (surface-of mgr id)))))]
   [:dependent-active (fn [mgr id _ _] (boolean (seq (active-dependents mgr id))))]
   [:in-flight        (fn [mgr id _ _] (not (claim-eviction! mgr id)))]])

(defn- refusal-reason
  "The first guard in `eviction-guards` that refuses, or nil to proceed. The
   :in-flight guard CLAIMS the eviction when it lets it through, so it must stay
   last."
  [mgr id st force?]
  (some (fn [[reason refuses?]] (when (refuses? mgr id st force?) reason)) eviction-guards))

(defn- do-evict! [mgr id force?]
  (let [st (state mgr id)]
    (if (nil? st)
      (refused-eviction id :unknown :errors [(str "no governed addon " (pr-str id))])
      (if-let [reason (refusal-reason mgr id st force?)]
        (refused-eviction id reason)
        (do
          (learn-surface! mgr id)
          (let [closed (part/close-owner! id)
                res    (r/try-effect (lport/-unmount! (:host mgr) id))
                out    (if (r/err? res) {:ok? false :errors [(:message res)]} (:ok res))
                errs   (when-not (:ok? out) (vec (:errors out ["unmount failed"])))]
            (update-state! mgr id #(-> % (assoc :phase :dormant :in-flight 0)
                                       (update :evictions inc)
                                       (assoc :last-error (first errs))))
            (install-stubs! mgr id)
            (when-let [f (get-in mgr [:opts :on-evicted])] (r/rescue nil (f id)))
            (cond-> (released-eviction id out :parts-closed (vec closed))
              (seq errs) (assoc :errors errs))))))))

(defn evict!
  "Release ID: shut it down, withdraw its surface, advertise stubs. Refuses while
   it is in flight, an active addon depends on it, it is pinned, or it has no
   surface a stub could advertise (see `eviction-guards`). OPTS {:force? true}
   overrides :pinned and :no-surface. Returns an EvictionReport; a refusal is
   :refused? true / :ok? false with its :reason."
  ([mgr id] (evict! mgr id {}))
  ([mgr id {:keys [force?]}]
   (let [res (with-lock mgr (fn [m] (do-evict! m id (boolean force?))))]
     (if (r/err? res)
       (refused-eviction id :lock-timeout :errors [(:message res)])
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
  "Sweep every :sweep-interval-ms on a daemon thread. Idempotent. Each tick
   sweeps the CURRENT manager (see `current`), never the value captured here."
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
                                  ^Runnable (fn [] (r/rescue nil (sweep! (current mgr))))
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
   addons under the resolved policy — the manifest's :addon/lifecycle under the
   manager's defaults and overrides. Returns the adopted ids.

   An adopted addon that nothing could advertise while dormant cannot be :lazy:
   it is governed as :eager with :downgraded :no-surface (exactly as boot! does),
   until a surface is learned for it."
  [mgr specs]
  (let [known (set (map :addon/id @(:specs mgr)))]
    (swap! (:specs mgr) into (remove #(contains? known (:addon/id %)) specs)))
  (let [lcs (lifecycles mgr)
        t   (now mgr)]
    (into [] (map (fn [{:keys [addon/id]}]
                    (let [[s src] (surface-of mgr id)
                          lc      (get lcs id)
                          no-surf (and (= :lazy (:policy lc)) (not (policy/lazy-permitted? s)))]
                      (swap! (:states mgr) update id
                             (fn [st]
                               (cond-> (assoc (or st (fresh-state id lc))
                                              :phase :active :last-used-ms t
                                              :surface/source src)
                                 no-surf (-> (assoc-in [:lifecycle :policy] :eager)
                                             (assoc :downgraded :no-surface))))))
                    id))
          specs)))

(defn forget!
  "Stop governing IDS entirely — the inverse of adopt!, for an addon being
   plugged OUT. Withdraws any stubs advertised for them and drops their spec,
   state and cached surface. Does NOT shut anything down: the caller tears the
   addon down first. Returns the ids that were governed."
  [mgr ids]
  (let [res (with-lock mgr
              (fn [m]
                (let [governed (into [] (filter #(state m %)) ids)
                      gone     (set ids)]
                  (doseq [id governed]
                    (r/rescue nil (lport/-remove-stubs! (:host m) id)))
                  (swap! (:specs m) (fn [ss] (into [] (remove #(contains? gone (:addon/id %))) ss)))
                  (swap! (:states m) #(apply dissoc % ids))
                  (swap! (:surfaces m) #(apply dissoc % ids))
                  governed)))]
    (if (r/err? res) [] (:ok res))))

(defn set-policy!
  "Change ID's lifecycle at runtime (e.g. pin it). Returns the new Lifecycle.

   Making an addon :lazy that has no surface a stub could advertise is REFUSED
   unless OPTS {:force? true}: the lifecycle is left as it was and returned with
   :refused :no-surface, because a lazy surface-less addon, once evicted, could
   never be woken again."
  ([mgr id decl] (set-policy! mgr id decl {}))
  ([mgr id decl {:keys [force?]}]
   (if (and (not force?)
            (= :lazy (:policy decl))
            (not (policy/lazy-permitted? (first (surface-of mgr id)))))
     (assoc (:lifecycle (state mgr id)) :refused :no-surface)
     (-> (swap! (:states mgr) update-in [id :lifecycle] merge decl)
         (get-in [id :lifecycle])))))

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
  "Make MGR the installed manager and the source of the dormancy oracle. MGR is
   also SEATED: it becomes `current` for every Manager sharing its state, so a
   host that swapped :host by assoc and re-installed is honoured by later calls.
   Prefer reseat-host!, which does this under the manager lock."
  [mgr]
  (some-> (:seat mgr) (reset! mgr))
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
;; Re-seating on a new host
;; =============================================================================

(defn- dormant-ids
  "Pure: the governed ids, in spec order, that are not mounted."
  [specs states]
  (into [] (comp (map :addon/id)
                 (filter #(contains? #{:dormant :failed} (get-in states [% :phase]))))
        specs))

(defn- next-host
  "The host to seat: HOST-FN itself when it already is an ILifecycleHost, else
   (HOST-FN old-host)."
  [host-fn old-host]
  (if (satisfies? lport/ILifecycleHost host-fn) host-fn (host-fn old-host)))

(defn- shares-state? [a b] (and a b (identical? (:lock a) (:lock b))))

(defn- do-reseat! [m host-fn]
  (let [h (r/try-effect (next-host host-fn (:host m)))]
    (cond
      (r/err? h)
      {:ok? false :reseated? false :errors [(str "host-fn failed: " (:message h))]}

      (not (satisfies? lport/ILifecycleHost (:ok h)))
      {:ok? false :reseated? false :errors ["host-fn did not answer an ILifecycleHost"]}

      :else
      (let [seat  (or (:seat m) (atom nil))
            nm    (assoc m :host (:ok h) :seat seat)
            inst  (installed-manager)
            inst? (or (nil? inst) (shares-state? inst m))
            sw    @(:sweeper m)
            ids   (dormant-ids @(:specs nm) @(:states nm))]
        (reset! seat nm)
        (when inst? (install! nm))
        (when sw
          (stop-sweeper! m)
          (start-sweeper! nm {:interval-ms (:interval-ms sw)}))
        (doseq [id ids] (install-stubs! nm id))
        {:ok?            true
         :reseated?      true
         :installed?     (boolean inst?)
         :sweeper-moved? (boolean sw)
         :rearmed        (filterv #(some? (first (surface-of nm %))) ids)}))))

(defn reseat-host!
  "Govern MGR's addons through a new ILifecycleHost, in place. HOST-FN is the new
   host, or (fn [old-host] new-host).

   Under the manager lock it builds a Manager on the new host that SHARES MGR's
   specs, states, surfaces, lock and sweeper, seats it (see `current`), installs
   it when MGR's state is the installed one (or nothing is installed), moves a
   running sweeper onto it and re-arms the stubs of every dormant addon on the
   new host. The old host is not called. Every activation, eviction and forget!
   resolves the seated manager once it holds the lock, so none that starts after
   this returns can reach the old host, and none running when it is called is cut
   short: the reseat waits for it.

   Returns a ReseatReport (hive-addon.lifecycle.schema/ReseatReport); on success
   (current mgr) is the new Manager."
  [mgr host-fn]
  (let [res (with-lock mgr #(do-reseat! % host-fn))]
    (if (r/err? res)
      {:ok? false :reseated? false :errors [(:message res)]}
      (:ok res))))

;; =============================================================================
;; A host over IMountHost, for tests and hosts without a tool surface
;; =============================================================================

(defrecord MountHost [mount-host mount-opts stubs]
  lport/ILifecycleHost
  (-mount! [_ specs peers]
    (boundary/mount! {:ordered specs} mount-host (assoc mount-opts :peer-specs peers)))
  (-unmount! [_ id]
    ;; "Shut down ... and forget the instance": teardown, then plug out through
    ;; the optional IMountUnregister port (a host without it keeps an inert
    ;; entry, which the next mount! replaces).
    (let [td (boundary/teardown! mount-host [id])
          un (boundary/unregister! mount-host [id])
          es (into (vec (:errors td)) (:errors un))]
      {:ok? (empty? es) :errors es
       :torn-down (vec (:torn-down td))
       :teardown/data-preserved? (:teardown/data-preserved? td)}))
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
