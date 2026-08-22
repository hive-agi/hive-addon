(ns hive-addon.hot
  "Hot-reload for IAddon instances — the public surface a host consumes.

   Reloading a namespace updates VARS. It does not update the addon INSTANCE the
   host registered at mount time, nor the sibling instances that were injected
   into its dependents' configs. Those objects were captured when they were
   constructed and are frozen; a namespace reload leaves the system holding old
   objects that call new code, which is the silent-corruption shape. What this
   namespace does is reconstruct them from the one thing a reload cannot
   invalidate: the MountSpec, which carries `:addon/init-ns` + `:addon/init-fn`
   as data. Remount is therefore the ordinary mount pipeline re-run over the
   affected slice, not a second registry.

   Layout:
     hive-addon.hot.source    — where an addon's source lives (:local/root vs jar)
     hive-addon.hot.cascade   — pure: which addons a reload touches, in what order
     hive-addon.hot.strategy  — OCP: how each addon is reloaded
     hive-addon.hot           — this facade: wiring, triggering, reporting

   hive-hot is a SOFT dependency, resolved through the var at call time and never
   required. Without it every function here still works — `reload-addon!` falls
   back to `require :reload` — and `hot!` degrades to a report saying
   :hot/available? false instead of throwing. Consumers that want file-watching
   add io.github.hive-agi/hive-hot themselves.

   THE ONE HARD RULE: never reload hive-addon.protocol. Reloading a
   protocol-defining namespace mints a new protocol var, and every live addon
   instance — same class NAME, different class OBJECT — stops satisfying it, so
   dispatch misses with an error that reads as if the method were never
   implemented. `no-reload` is that rule as data; feed it to hive-hot's
   :no-reload and the hazard cannot be tripped."
  (:require [hive-addon.hot.cascade :as cascade]
            [hive-addon.hot.source :as source]
            [hive-addon.hot.strategy :as strategy]
            [hive-addon.mount.port :as port]
            [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; The protocol-reload interlock
;; =============================================================================

(def no-reload
  "Namespaces that must NEVER be reloaded while addons are live, for hive-hot's
   :no-reload option.

   hive-addon.protocol defines IAddon. Reloading it replaces the protocol var;
   instances built against the previous var keep their old class object and stop
   satisfying the new protocol, producing \"No implementation of method ... for
   class X\" for a class whose name matches perfectly. The mount and hot
   namespaces are pinned for the same reason — they hold the registry state and
   the strategy chain a reload would otherwise reset mid-flight."
  '#{hive-addon.protocol
     hive-addon.mount.port
     hive-addon.hot
     hive-addon.hot.strategy})

;; =============================================================================
;; hive-hot — soft resolution
;; =============================================================================

(defn- hot-var
  "Resolve a hive-hot var, or nil when hive-hot is not on the classpath.
   Resolved at CALL time so hive-hot appearing later is picked up."
  [sym]
  (try (requiring-resolve sym) (catch Throwable _ nil)))

(defn available?
  "Is hive-hot on the classpath?"
  []
  (some? (hot-var 'hive-hot.core/reg-hot)))

(defn- require-reload!
  "Fallback namespace reloader for when hive-hot is absent: reload each namespace
   directly. No dependency cascade at the namespace level — that is precisely
   what hive-hot/clj-reload provides and this substitute does not."
  [ns-strs]
  (reduce (fn [acc ns-str]
            (let [res (r/try-effect (require (symbol ns-str) :reload))]
              (if (r/err? res)
                (reduced (assoc acc :failed (symbol ns-str) :error (:message res)))
                (update acc :loaded conj (symbol ns-str)))))
          {:loaded []}
          ns-strs))

(defn ns-reloader
  "The namespace-level reloader injected into strategies.

   Prefers hive-hot's clj-reload-backed `reload!`, which reloads every CHANGED
   namespace and cascades to their dependents properly. Falls back to plain
   `require :reload` over the given namespaces when hive-hot is absent."
  []
  (if-let [reload! (hot-var 'hive-hot.core/reload!)]
    (fn [_ns-strs] (reload!))
    require-reload!))

;; =============================================================================
;; Context
;; =============================================================================

(def mount-opt-keys
  "Keys that belong to boundary/mount!, not to a reload's own options.

   A caller that passes one of these at the TOP level of reload opts means it
   for mount!. Silently ignoring it is the worst available outcome: mount! falls
   back to resolve-config-default, the addon still constructs, still initializes
   and still reports :success? true — while coming back DEGRADED, missing the
   config.edn merge and whatever host adapters its real config carried. Measured
   in hive-mcp: a remount that lost :resolve-config left hive.carto :active with
   zero runtime ports and silently dropped its whole MCP subdomain.

   So they are FOLDED into :mount-opts rather than dropped."
  #{:resolve-config :init-retry :license-gate :on-event :sleep-fn :peer-specs})

(defn- reload-ctx
  [host specs spec seeds {:keys [trigger changed-ns ns-reloaded? mount-opts
                                 solve-opts strategies reload-ns!]
                          :as opts}]
  (let [stray (select-keys opts mount-opt-keys)]
    {:hot/host host
     :hot/specs specs
     :hot/seeds seeds
     :hot/source (source/spec-source spec)
     :hot/trigger (or trigger :manual)
     :hot/changed-ns changed-ns
     :hot/ns-reloaded? (boolean ns-reloaded?)
     ;; stray FIRST so an explicit :mount-opts entry still wins.
     :hot/mount-opts (merge stray (or mount-opts {}))
     :hot/solve-opts (or solve-opts {})
     :hot/strategies strategies
     :hot/reload-ns! (or reload-ns! (ns-reloader))}))

(defn- not-found-report
  [addon-id opts]
  {:hot/trigger (or (:trigger opts) :manual)
   :hot/strategy :hot/none
   :hot/seeds #{}
   :hot/affected []
   :hot/torn-down []
   :teardown/data-preserved? true
   :mounted []
   :ok? false
   :errors [(str "no mounted spec with :addon/id " (pr-str addon-id))]})

(defn- merge-reports
  "Fold per-strategy reports into one. Used when a single trigger seeds addons
   that resolve to different strategies — each group runs its own strategy and
   the outcomes are reported together rather than one shadowing the others."
  [reports opts]
  (if (= 1 (count reports))
    (first reports)
    {:hot/trigger (or (:trigger opts) :manual)
     :hot/strategy (if-let [ss (seq (distinct (map :hot/strategy reports)))]
                     (if (= 1 (count ss)) (first ss) :hot/mixed)
                     :hot/none)
     :hot/changed-ns (:changed-ns opts)
     :hot/seeds (into #{} (mapcat :hot/seeds) reports)
     :hot/affected (into [] (comp (mapcat :hot/affected) (distinct)) reports)
     :hot/torn-down (into [] (mapcat :hot/torn-down) reports)
     :teardown/data-preserved? true
     :mounted (into [] (mapcat :mounted) reports)
     :ok? (every? :ok? reports)
     :errors (into [] (mapcat #(or (:errors %) [])) reports)}))

;; =============================================================================
;; Triggering a reload
;; =============================================================================

(defn reload-seeds!
  "Reload `seed-ids` and every addon that depends on them.

   Seeds are grouped by the strategy each one selects, so a system mixing
   strategies reloads each group under its own policy exactly once — rather than
   letting one seed's strategy decide for addons that asked for something else.

   Returns a RemountReport."
  [host specs seed-ids & [opts]]
  (let [opts   (or opts {})
        by-id  (into {} (map (juxt :addon/id identity)) specs)
        seeds  (into #{} (filter by-id) seed-ids)]
    (if (empty? seeds)
      (not-found-report seed-ids opts)
      (let [chain (or (:strategies opts) (strategy/installed-strategies))
            ;; Group by the strategy each seed selects, under its own context.
            grouped (group-by
                     (fn [id]
                       (let [spec (by-id id)
                             ctx  (reload-ctx host specs spec #{id} opts)
                             sel  (strategy/select spec ctx chain)]
                         (if (r/err? sel) :hot/none (strategy/-strategy-id (:ok sel)))))
                     (sort seeds))
            reports (mapv (fn [[_sid ids]]
                            (let [group (set ids)
                                  spec  (by-id (first (sort group)))
                                  ctx   (reload-ctx host specs spec group opts)]
                              (strategy/reload! spec ctx)))
                          grouped)]
        (merge-reports reports opts)))))

(defn reload-addon!
  "Reload ONE addon by id, cascading to its dependents. This is what the
   `hive hot reload <addon-id>` command drives.

   opts: {:trigger        :manual | :ns-reload | :file-change
          :ns-reloaded?   true when the namespaces were already reloaded
          :mount-opts     forwarded to mount! (e.g. :resolve-config, :license-gate)
          :solve-opts     forwarded to solve  (e.g. :rules)
          :strategies     strategy chain override
          :reload-ns!     namespace-reloader override}

   Returns a RemountReport."
  [host specs addon-id & [opts]]
  (reload-seeds! host specs #{addon-id} opts))

(defn reload-namespace!
  "Reload every addon whose constructor namespace is `ns-str`, plus dependents.
   This is what a hive-hot component callback drives, so :ns-reloaded? defaults
   to true — clj-reload has already done the namespace work by then."
  [host specs ns-str & [opts]]
  (let [seeds (cascade/seeds-for-ns specs ns-str)]
    (reload-seeds! host specs seeds
                   (merge {:trigger :ns-reload
                           :changed-ns (str ns-str)
                           :ns-reloaded? true}
                          opts))))

(defn reload-all!
  "Reload every mounted addon, in dependency order."
  [host specs & [opts]]
  (reload-seeds! host specs (into #{} (map :addon/id) specs) opts))

;; =============================================================================
;; Wiring into hive-hot
;; =============================================================================

(defn- registration
  [spec src sid]
  {:addon/id (:addon/id spec)
   :hot/component-id (keyword "addon" (str (:addon/id spec)))
   :addon/init-ns (:addon/init-ns spec)
   :hot/strategy-id sid
   :hot/source-kind (:hot/source-kind src)
   :hot/reloadable? (boolean (:hot/reloadable? src))})

(defn- selected-strategy-id
  [host specs spec opts]
  (let [ctx (reload-ctx host specs spec #{(:addon/id spec)} opts)
        sel (strategy/select spec ctx (or (:strategies opts)
                                          (strategy/installed-strategies)))]
    (if (r/err? sel) :hot/none (strategy/-strategy-id (:ok sel)))))

(defn plan
  "What `hot!` WOULD wire, with no effects — the dry-run parity of hot!.

   Reports, per spec, the strategy that would be selected, where its source lives
   and whether it is reloadable at all. Use it to answer \"why is this addon not
   hot-reloading?\" without touching the running system."
  [host specs & [opts]]
  (let [opts (or opts {})
        rows (mapv (fn [spec]
                     (let [src (source/spec-source spec)
                           sid (selected-strategy-id host specs spec opts)]
                       (assoc (registration spec src sid) :hot/source src)))
                   specs)
        {reloadable true skipped false} (group-by :hot/reloadable? rows)]
    {:hot/available? (available?)
     :hot/registered (vec (or reloadable []))
     :hot/skipped (mapv :hot/source (or skipped []))
     :hot/dirs (source/watchable-dirs specs)
     :hot/no-reload no-reload
     :ok? true}))

(defn hot!
  "Wire `specs` into hive-hot so that reloading an addon's constructor namespace
   remounts that addon and its dependents.

   Registers one hive-hot component per reloadable addon, keyed :addon/<id>.
   Jar-backed and source-absent addons are NOT registered — they are reported
   under :hot/skipped, because registering a component whose bytes cannot change
   would only produce reloads that appear to succeed while changing nothing.

   Degrades gracefully: with hive-hot absent this performs no effects and returns
   a report with :hot/available? false.

   Returns a HotReport. Its :hot/dirs and :hot/no-reload are what to pass on to
   hive-hot's own init:

     (let [{:keys [hot/dirs hot/no-reload]} (hot! host specs)]
       (hive-hot.core/init-with-watcher! {:dirs (vec dirs) :no-reload no-reload}))"
  [host specs & [opts]]
  (let [opts    (or opts {})
        preview (plan host specs opts)
        reg-hot (hot-var 'hive-hot.core/reg-hot)]
    (if-not reg-hot
      (assoc preview :hot/available? false :hot/registered [])
      (let [errors (into []
                         (keep (fn [{:keys [addon/id hot/component-id addon/init-ns]}]
                                 (let [res (r/try-effect
                                            (reg-hot component-id
                                                     {:ns (symbol init-ns)
                                                      :on-reload
                                                      (fn []
                                                        (reload-namespace! host specs init-ns opts))
                                                      :on-error
                                                      (fn [_ex] nil)}))]
                                   (when (r/err? res)
                                     (str id ": " (:message res))))))
                         (:hot/registered preview))]
        (cond-> (assoc preview :hot/available? true :ok? (empty? errors))
          (seq errors) (assoc :errors errors))))))

(defn unhot!
  "Deregister every addon in `specs` from hive-hot. No-op without hive-hot."
  [specs]
  (if-let [unreg (hot-var 'hive-hot.core/unreg-hot)]
    (do (doseq [spec specs]
          (r/try-effect (unreg (keyword "addon" (str (:addon/id spec))))))
        {:hot/available? true :ok? true
         :hot/unregistered (mapv :addon/id specs)})
    {:hot/available? false :ok? true :hot/unregistered []}))

(defn status
  "Current hot-reload status: hive-hot availability, its component registry, and
   the installed strategy chain."
  []
  (let [hot-status (hot-var 'hive-hot.core/status)]
    {:hot/available? (available?)
     :hot/strategies (mapv strategy/-strategy-id (strategy/installed-strategies))
     :hot/no-reload no-reload
     :hot/hive-hot (when hot-status (hot-status))}))

;; =============================================================================
;; Re-exports — the facade surface
;; =============================================================================
;; IReloadStrategy is NOT re-exported: a protocol cannot be plain-def aliased.
;; Implement it from its canonical home, hive-addon.hot.strategy.

;; Re-exports DELEGATE through the var at call time; they are never plain
;; `def` aliases. A `def` alias snapshots the function object, so reloading the
;; namespace it came from leaves this facade calling the PREVIOUS one — which,
;; for anything protocol-backed, hands back instances of a class the current
;; protocol has never heard of. That is the same Capture-by-Var defect this
;; library exists to fix, and in a hot-reload facade it would be self-defeating.

(defn dependents
  "hive-addon.hot.cascade/dependents — transitive dependent closure of seeds."
  ([specs seed-ids] (cascade/dependents specs seed-ids))
  ([specs seed-ids opts] (cascade/dependents specs seed-ids opts)))

(defn affected-plan
  "hive-addon.hot.cascade/affected-plan — the ordered slice a reload drives."
  ([specs seed-ids] (cascade/affected-plan specs seed-ids))
  ([specs seed-ids opts] (cascade/affected-plan specs seed-ids opts)))

(defn spec-source
  "hive-addon.hot.source/spec-source — where a spec's constructor ns lives."
  [spec]
  (source/spec-source spec))

(defn watchable-dirs
  "hive-addon.hot.source/watchable-dirs — the :local/root source dirs to watch."
  [specs]
  (source/watchable-dirs specs))

(defn register-strategy!
  "hive-addon.hot.strategy/register-strategy! — OCP: add a reload strategy."
  [strategy]
  (strategy/register-strategy! strategy))

(defn install-strategies!
  "hive-addon.hot.strategy/install-strategies! — replace the strategy chain."
  [strategies]
  (strategy/install-strategies! strategies))

(defn installed-strategies
  "hive-addon.hot.strategy/installed-strategies — the live strategy chain."
  []
  (strategy/installed-strategies))

(defn reset-strategies!
  "hive-addon.hot.strategy/reset-strategies! — restore the built-in chain."
  []
  (strategy/reset-strategies!))

(defn default-strategies
  "hive-addon.hot.strategy/default-strategies — a FRESH built-in chain, rebuilt
   against the current protocol var."
  []
  (strategy/default-strategies))
