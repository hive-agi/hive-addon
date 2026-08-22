(ns hive-addon.hot.strategy
  "How an addon's new code is brought into a live system — the OCP seam of the
   hot-reload bridge.

   The set of reload strategies is OPEN. Most addons want the default
   (`:remount`: shut down, reconstruct from the manifest, re-initialize, cascade
   to dependents), but an addon with particular needs — an open socket it must
   drain, a native handle it cannot re-acquire, a stateless dispatcher that only
   needs its vars refreshed — supplies its own. So strategies are a PROTOCOL with
   a rule chain, never a `case` over a closed keyword set: closing this set would
   be the defect, and adding a strategy must never mean editing this namespace.

   Selection, in order:
   1. A spec that DECLARES `:addon/reload-strategy <id>` gets exactly that
      strategy, looked up by id. A declared-but-unregistered id is an error, not
      a silent fallback to the default — an addon that asked for special handling
      and quietly got generic handling is worse than one that refused.
   2. Otherwise the first chain member whose `-applies?` answers true.
   The built-in chain ends in a catch-all, so selection always succeeds.

   Registration is a var-held chain plus an install seam, mirroring the licence
   gate in hive-addon.mount.entitlement. Strategies are resolved from the var at
   CALL time, never captured at wiring time.

   Effectful: this is the stratum that shuts addons down and mounts them again.
   The namespace-level reloader is INJECTED (`:hot/reload-ns!`) rather than
   required, so hive-hot stays a soft dependency."
  (:require [hive-addon.hot.cascade :as cascade]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.port :as port]
            [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; IReloadStrategy — the open dispatch
;; =============================================================================

(defprotocol IReloadStrategy
  "One way of bringing an addon's new code into a running system."
  (-strategy-id [this]
    "Keyword identifying this strategy. What a spec names in
     `:addon/reload-strategy`. Must be stable — it is a manifest-level contract.")
  (-applies? [this spec ctx]
    "Should this strategy handle `spec` given reload context `ctx`, when the spec
     declared no explicit strategy? Chain order decides ties; the built-in
     catch-all answers true unconditionally and sits last.")
  (-reload! [this spec ctx]
    "Perform the reload. Returns a RemountReport map. MUST NOT throw — a failure
     is folded into the report, because a reload that explodes takes the whole
     watcher thread with it."))

;; =============================================================================
;; Report construction
;; =============================================================================

(defn- base-report
  "The invariant skeleton every strategy's report shares. :teardown/data-preserved?
   is true by construction — no strategy is permitted to delete data."
  [strategy-id ctx]
  {:hot/trigger (:hot/trigger ctx :manual)
   :hot/strategy strategy-id
   :hot/changed-ns (:hot/changed-ns ctx)
   :hot/seeds (set (:hot/seeds ctx))
   :hot/affected []
   :hot/torn-down []
   :teardown/data-preserved? true
   :mounted []
   :ok? true})

(defn- refusal
  "A report that declines to reload, without touching the running system."
  [strategy-id ctx errors]
  (assoc (base-report strategy-id ctx)
         :ok? false
         :errors (vec errors)))

;; =============================================================================
;; Namespace-level reload — injected, never required
;; =============================================================================

(defn- reload-namespaces!
  "Run the injected namespace reloader, if the caller supplied one and the
   namespaces have not already been reloaded (a hive-hot component callback fires
   AFTER clj-reload has done the ns work, so doing it again would be waste).

   Returns {:ok? bool :loaded [...] :errors [...]}."
  [ctx ns-strs]
  (cond
    (:hot/ns-reloaded? ctx)
    {:ok? true :loaded (vec ns-strs)}

    (nil? (:hot/reload-ns! ctx))
    {:ok? true :loaded []}

    :else
    (let [res (r/try-effect ((:hot/reload-ns! ctx) ns-strs))]
      (if (r/err? res)
        {:ok? false :loaded [] :errors [(str "namespace reload failed: " (:message res))]}
        (let [{:keys [loaded failed] :as out} (:ok res)]
          (cond-> {:ok? (nil? failed) :loaded (vec loaded)}
            failed (assoc :errors [(str "namespace reload failed at: " failed
                                        (when-let [e (:error out)] (str " — " e)))])))))))

;; =============================================================================
;; Built-in strategy: :remount (the default)
;; =============================================================================

(defn- mount-errors
  "Per-addon failures from a MountReport, flattened and attributed.

   A MountReport carries NO top-level :errors — every failure lives on the
   individual MountResult. Reading (:errors report) therefore yields nil even for
   a report whose :ok? is false, which would produce the worst possible artifact:
   a reload that says it failed and cannot say why. A failed result with an empty
   :errors still contributes a line, named by the phase it died in."
  [report]
  (into []
        (comp (remove :success?)
              (mapcat (fn [{:keys [addon/id errors phase]}]
                        (if (seq errors)
                          (map #(str id ": " %) errors)
                          [(str id ": failed at phase " phase)]))))
        (:mounted report)))

(defrecord RemountStrategy []
  IReloadStrategy
  (-strategy-id [_] :remount)
  (-applies? [_ _spec _ctx] true)
  (-reload! [this spec ctx]
    (let [id         (:addon/id spec)
          specs      (:hot/specs ctx)
          host       (:hot/host ctx)
          seeds      (set (:hot/seeds ctx #{id}))
          solve-opts (:hot/solve-opts ctx {})
          plan       (cascade/affected-plan specs seeds solve-opts)
          ordered    (:ordered plan)
          ids        (mapv :addon/id ordered)
          ns-res     (reload-namespaces! ctx (distinct (keep :addon/init-ns ordered)))]
      (if-not (:ok? ns-res)
        ;; Code that will not load must not take the running system down: refuse
        ;; BEFORE any teardown, leaving every live instance in place.
        (assoc (refusal (-strategy-id this) ctx (:errors ns-res))
               :hot/affected ids)
        ;; Reverse-order shutdown, then re-drive the ORDINARY mount pipeline over
        ;; the affected slice. mount! re-resolves each constructor at call time,
        ;; so the freshly loaded code is what gets constructed. :peer-specs keeps
        ;; sibling injection resolving against the WHOLE system, not the slice.
        (let [td     (boundary/teardown! host ids)
              report (boundary/mount! plan host
                                      (assoc (:hot/mount-opts ctx {})
                                             :peer-specs specs))
              errors (into (vec (:errors td)) (mount-errors report))]
          (cond-> (assoc (base-report (-strategy-id this) ctx)
                         :hot/affected ids
                         :hot/torn-down (vec (:torn-down td))
                         :hot/ns-reloaded (vec (:loaded ns-res))
                         :mounted (vec (:mounted report))
                         :ok? (and (:ok? report) (empty? (:errors td))))
            (seq (:cycles plan)) (assoc :hot/cycles (:cycles plan))
            (seq errors)         (assoc :errors (vec errors))))))))

;; =============================================================================
;; Built-in strategy: :restart-required
;; =============================================================================

(defrecord RestartRequiredStrategy []
  IReloadStrategy
  (-strategy-id [_] :restart-required)
  (-applies? [_ _spec ctx]
    ;; A jar-backed or absent addon has no source that can change under a running
    ;; JVM. Reloading it would reconstruct the SAME code and report success —
    ;; the most misleading outcome available. Refuse instead.
    (not (:hot/reloadable? (:hot/source ctx) true)))
  (-reload! [this spec ctx]
    (let [src  (:hot/source ctx)
          kind (:hot/source-kind src)]
      (refusal (-strategy-id this) ctx
               [(str (:addon/id spec) " is not hot-reloadable: its constructor namespace "
                     (:addon/init-ns spec)
                     (case kind
                       :jar    (str " is inside a JAR (" (:hot/source-url src)
                                    "). Wire it as a :local/root dep in local.deps.edn"
                                    " to make its source reloadable, or restart the host.")
                       :absent " has no source on the classpath (AOT-only or generated). Restart the host."
                       (str " reports source-kind " kind ". Restart the host.")))]))))

;; =============================================================================
;; Built-in strategy: :in-place (opt-in only)
;; =============================================================================

(defrecord InPlaceStrategy []
  IReloadStrategy
  (-strategy-id [_] :in-place)
  ;; Never auto-selected — an addon opts in with :addon/reload-strategy :in-place.
  (-applies? [_ _spec _ctx] false)
  (-reload! [this spec ctx]
    (let [id     (:addon/id spec)
          ns-res (reload-namespaces! ctx [(:addon/init-ns spec)])]
      (cond-> (assoc (base-report (-strategy-id this) ctx)
                     :hot/affected [id]
                     :hot/ns-reloaded (vec (:loaded ns-res))
                     :ok? (:ok? ns-res))
        (seq (:errors ns-res)) (assoc :errors (vec (:errors ns-res)))))))

;; =============================================================================
;; Built-in strategy: :inert
;; =============================================================================

(defrecord InertStrategy []
  IReloadStrategy
  (-strategy-id [_] :inert)
  (-applies? [_ _spec _ctx] false)
  (-reload! [this spec ctx]
    ;; For an addon that must survive reloads untouched (holds a process, a
    ;; terminal, a long-lived external session). Declares itself out of the
    ;; cascade; reports success because doing nothing WAS the contract.
    (assoc (base-report (-strategy-id this) ctx)
           :hot/affected []
           :hot/ns-reloaded []
           :hot/skipped-id (:addon/id spec))))

;; =============================================================================
;; The chain — default, installable, extensible (OCP)
;; =============================================================================

(defn default-strategies
  "A FRESH built-in strategy chain.

   Deliberately a function, not a def. Records built at load time and held in a
   `defonce` would keep the class objects of whichever protocol var was current
   when they were constructed; reloading this namespace re-mints IReloadStrategy
   and those held instances stop satisfying it — the same class-identity failure
   this whole namespace exists to prevent, turned on itself. Rebuilding on every
   call means the chain always matches the live protocol.

   ORDER IS SIGNIFICANT: selection takes the first member whose -applies? is
   true, and RemountStrategy is the unconditional catch-all, so it stays last.
   :in-place and :inert never auto-apply — they exist to be NAMED by a spec's
   :addon/reload-strategy."
  []
  [(->RestartRequiredStrategy)
   (->InPlaceStrategy)
   (->InertStrategy)
   (->RemountStrategy)])

;; Module-registered strategies. Only the OVERRIDES live here — never the
;; built-ins, which are rebuilt per call. {:replace <chain-or-nil> :extra [...]}.
(defonce ^:private overrides
  (atom {:replace nil :extra []}))

(defn- live-strategies
  "Drop entries that no longer satisfy IReloadStrategy.

   A module's strategy is orphaned the moment ITS namespace is reloaded, exactly
   as a stale addon instance is. Filtering here degrades to the built-in
   behaviour for that addon instead of throwing a dispatch error from inside a
   file-watcher thread, where nobody would see it."
  [strategies]
  (filterv #(satisfies? IReloadStrategy %) strategies))

(defn installed-strategies
  "The live strategy chain: module-registered strategies first, then the
   built-ins, catch-all last. Rebuilt on every call, so a strategy registered
   after wiring is seen, and a strategy orphaned by a reload is dropped."
  []
  (let [{:keys [replace extra]} @overrides]
    (if replace
      (live-strategies replace)
      (let [customs (live-strategies extra)
            taken   (set (map -strategy-id customs))]
        (vec (concat customs
                     (remove #(contains? taken (-strategy-id %))
                             (default-strategies))))))))

(defn install-strategies!
  "Replace the WHOLE chain, built-ins included. Keep a catch-all last or
   selection can fail. Pass nil to fall back to registered-plus-built-in."
  [strategies]
  (swap! overrides assoc :replace (when strategies (vec strategies)))
  (installed-strategies))

(defn register-strategy!
  "Add one strategy, taking precedence over the built-ins. This is the OCP entry
   point: a module ships its own IReloadStrategy and registers it at load time
   without this namespace changing.

   Replaces any existing registration with the same -strategy-id, so re-running
   it (as a namespace reload does) is idempotent rather than accumulating
   duplicates."
  [strategy]
  (let [sid (-strategy-id strategy)]
    (swap! overrides update :extra
           (fn [chain]
             (conj (vec (remove #(and (satisfies? IReloadStrategy %)
                                      (= sid (-strategy-id %)))
                                chain))
                   strategy)))
    (installed-strategies)))

(defn reset-strategies!
  "Drop every override and registration, restoring the built-in chain. For tests
   and for recovering a fouled registry."
  []
  (reset! overrides {:replace nil :extra []})
  (installed-strategies))

;; =============================================================================
;; Selection
;; =============================================================================

(defn strategy-by-id
  "Look up a strategy in `chain` by its id, or nil."
  [chain sid]
  (first (filter #(= sid (-strategy-id %)) chain)))

(defn select
  "Choose the strategy for `spec` under `ctx`.

   Returns (r/ok strategy), or (r/err :hot/unknown-strategy ...) when the spec
   declared an id nothing in the chain provides. A declared strategy bypasses
   -applies? entirely: the addon asked for it explicitly."
  ([spec ctx] (select spec ctx (installed-strategies)))
  ([spec ctx chain]
   (if-let [declared (:addon/reload-strategy spec)]
     (if-let [found (strategy-by-id chain declared)]
       (r/ok found)
       (r/err :hot/unknown-strategy
              {:addon/id (:addon/id spec)
               :declared declared
               :available (mapv -strategy-id chain)}))
     (if-let [found (first (filter #(-applies? % spec ctx) chain))]
       (r/ok found)
       (r/err :hot/no-strategy
              {:addon/id (:addon/id spec)
               :available (mapv -strategy-id chain)})))))

;; =============================================================================
;; Drive
;; =============================================================================

(defn reload!
  "Select a strategy for `spec` and run it. Never throws: a selection failure and
   a strategy that blows up both come back as a RemountReport with :ok? false."
  [spec ctx]
  (let [chain (or (:hot/strategies ctx) (installed-strategies))
        sel   (select spec ctx chain)]
    (if (r/err? sel)
      (refusal :hot/none ctx
               [(str "no reload strategy for " (:addon/id spec) ": " (:message sel))])
      (let [strategy (:ok sel)
            res (r/try-effect (-reload! strategy spec ctx))]
        (if (r/err? res)
          (refusal (-strategy-id strategy) ctx
                   [(str "reload strategy " (-strategy-id strategy) " threw: "
                         (:message res))])
          (:ok res))))))
