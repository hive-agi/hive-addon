(ns hive-addon.hot.schema
  "Malli value objects for the IAddon hot-reload bridge.

   Shapes are uncompiled malli DATA (house idiom: PascalCase defs) seeded into a
   LOCAL composite registry that COMPOSES hive-addon.mount.schema's registry —
   AddonId/MountResult/MountSpec are reused, never redefined. The registry is
   NEVER installed as the malli global default; reach it via
   `schema`/`validate`/`explain`/`validate*` or by passing {:registry registry}.

   Hot shapes are registered under :hot/* keys (:hot/registration, :hot/report,
   :hot/remount-report, :hot/inject-report, :hot/eject-report, :hot/source,
   :hot/strategy-id, :hot/remount-outcome).

   Two invariants are carried as DATA rather than prose:
   - The reports' :teardown/data-preserved? is a :boolean VERDICT computed by
     hive-addon.hot.report/data-preserved? over the teardowns that actually
     released something — never true by construction. (The per-teardown
     TeardownReport in hive-addon.mount.schema still pins [:= true]: that is
     the no-nuke claim a teardown makes; the report verdict is whether every
     release made it.)
   - :hot/strategy-id is an open :keyword, never an enum. The strategy set is
     extensible by any module (OCP); closing it here would be the defect."
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr]
            [hive-addon.schema :as s]
            [hive-addon.mount.schema :as ms]
            [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Value-object schemas — uncompiled malli DATA
;; =============================================================================

(def HotTrigger
  "What caused a reload. :manual is an explicit call (an operator, or the
   `hive hot reload <addon-id>` MCP command); :ns-reload is a hive-hot component
   callback; :file-change is watcher-driven."
  [:enum :manual :ns-reload :file-change])

(def StrategyId
  "Identifier of a reload strategy. OPEN on purpose — modules register their own
   strategies (OCP), so this is a plain keyword, never an enum."
  :keyword)

(def SourceKind
  "Where an addon's constructor namespace physically lives, which decides whether
   its code can be reloaded at all.

   :directory — a real directory on the classpath (a `:local/root` dep, or this
                repo's own src). clj-reload can watch and reload it.
   :jar       — inside a JAR (an `:mvn/version` dep). The bytes cannot change
                without a restart, so hot-reload is meaningless.
   :absent    — the namespace's source file is not on the classpath at all."
  [:enum :directory :jar :absent])

(def AddonSource
  "Resolved physical source of an addon's constructor namespace. :hot/reloadable?
   is the decision derived from :hot/source-kind — true only for :directory. Open."
  [:map {:closed false}
   [:addon/id s/AddonId]
   [:addon/init-ns [:string {:min 1}]]
   [:hot/source-kind SourceKind]
   [:hot/reloadable? :boolean]
   [:hot/source-dir {:optional true} [:maybe :string]]
   [:hot/source-url {:optional true} [:maybe :string]]])

(def HotRegistration
  "One addon's registration into the hive-hot component registry.
   :hot/component-id is the key hive-hot knows it by; :addon/init-ns is the
   namespace whose reload triggers the strategy named by :hot/strategy-id. Open."
  [:map {:closed false}
   [:addon/id s/AddonId]
   [:hot/component-id :keyword]
   [:addon/init-ns [:string {:min 1}]]
   [:hot/strategy-id StrategyId]
   [:hot/source-kind SourceKind]
   [:hot/reloadable? :boolean]])

(def HotReport
  "Outcome of wiring a spec set into hive-hot. :hot/available? is false when
   hive-hot is absent from the classpath — the bridge then degrades to a no-op
   report rather than throwing, so a consumer without hive-hot still works.
   :hot/dirs is the watchable source-dir set derived from the specs (what a
   consumer feeds hive-hot's :dirs). :ok? is true only when every spec
   registered. Open."
  [:map {:closed false}
   [:hot/available? :boolean]
   [:hot/registered [:sequential HotRegistration]]
   [:hot/skipped [:sequential AddonSource]]
   [:hot/dirs [:set :string]]
   [:hot/no-reload [:set :symbol]]
   [:ok? :boolean]
   [:errors {:optional true} [:sequential :string]]])

(def RemountReport
  "Outcome of reloading an addon and its transitive dependents.

   :hot/seeds are the directly-changed addon ids; :hot/affected is the
   topo-ordered closure actually acted on (deps before dependents) and always
   CONTAINS the seeds. :hot/torn-down is the reverse-order shutdown that ran
   first. :mounted carries the per-addon MountResult from the ORDINARY mount
   pipeline — hot-reload is a projection of mount!, not a second registry.

   :hot/roots are the seeds' classpath source dirs the namespace reload was
   scoped to. :hot/ns-reloaded names every namespace the reloader loaded;
   :hot/ns-skipped the namespaces changed OUTSIDE the roots that it declined
   (they stay pending for their own root), :hot/ns-dragged the ones changed
   outside but loaded because they depend on a reloaded namespace, and
   :hot/ns-unchanged? that nothing under the roots had changed at all.
   :hot/widened is the addon ids that joined :hot/affected because their
   constructor namespace was loaded — absent when the reload stayed inside the
   requested slice, so \"nothing else changed\" and \"other addons were dragged
   in\" are distinguishable.

   :hot/multi-file names a loaded namespace found in more than one file, and
   :hot/stale-ctors the namespaces the reload reported loaded whose constructor
   var provably did not change — both are the \"reloaded, but the code did not\"
   shape, and the second REFUSES the remount.

   What is RUNNING afterwards is stated, not left to inference:
   :hot/refused? true means the remount was declined before any teardown
   (:hot/preflight carries the dry-run results that refused it); :hot/restored
   are ids whose new instance failed and whose PREVIOUS instance is live again;
   :hot/down are ids with neither. :hot/restored? is present only when
   something failed, true exactly when nothing is down.

   :hot/strategy names the strategy that ran. :teardown/data-preserved? is a
   COMPUTED verdict (hive-addon.hot.report/data-preserved?) over the teardowns
   that released something — no longer true by construction. Open."
  [:map {:closed false}
   [:hot/trigger HotTrigger]
   [:hot/strategy StrategyId]
   [:hot/changed-ns {:optional true} [:maybe [:string {:min 1}]]]
   [:hot/seeds [:set s/AddonId]]
   [:hot/roots {:optional true} [:sequential :string]]
   [:hot/affected [:sequential s/AddonId]]
   [:hot/torn-down [:sequential s/AddonId]]
   [:hot/cycles {:optional true} [:set s/AddonId]]
   [:hot/ns-reloaded {:optional true} [:sequential :string]]
   [:hot/ns-skipped {:optional true} [:sequential :string]]
   [:hot/ns-dragged {:optional true} [:sequential :string]]
   [:hot/ns-unchanged? {:optional true} :boolean]
   [:hot/multi-file {:optional true} [:map-of :string [:sequential :string]]]
   [:hot/stale-ctors {:optional true} [:sequential :string]]
   [:hot/widened {:optional true} [:set s/AddonId]]
   [:hot/refused? {:optional true} :boolean]
   [:hot/preflight {:optional true} [:sequential ms/MountResult]]
   [:hot/restored {:optional true} [:sequential s/AddonId]]
   [:hot/down {:optional true} [:sequential s/AddonId]]
   [:hot/restored? {:optional true} :boolean]
   [:teardown/data-preserved? :boolean]
   [:mounted [:sequential ms/MountResult]]
   [:ok? :boolean]
   [:errors {:optional true} [:sequential :string]]])

(def InjectReport
  "Outcome of injecting addons that were not on the classpath at boot
   (hive-addon.hot.inject/inject!).

   :hot/paths are the classpath entries the path stood for and :hot/classpath
   what extending the live loader with each answered. :hot/discovered is every
   addon whose manifest lives under those paths; :hot/already-mounted the ones
   the host already had (left alone — injection never resurrects or replaces),
   :hot/injected the ones actually mounted. :hot/affected is the ordered slice
   that ran: the injected addons plus every ALREADY-MOUNTED dependent that now
   has a new sibling to receive, which is torn down and remounted (listed in
   :hot/torn-down). :mounted carries the per-addon MountResult from the
   ordinary mount pipeline. :hot/dirs-added are the source dirs handed to
   hive-hot so the new addons reload like the rest; :hot/registered the ids
   registered as hive-hot components. :hot/remembered are the ids recorded in
   the injected-spec registry (so discovery from ANY thread sees them), and
   :hot/adopted the ids the installed lifecycle manager now governs under
   their manifest policy. :teardown/data-preserved? is computed over the
   dependent teardown that ran. Open."
  [:map {:closed false}
   [:hot/path :string]
   [:hot/paths [:sequential :string]]
   [:hot/classpath [:sequential [:map {:closed false}
                                 [:url :string]
                                 [:already? :boolean]]]]
   [:hot/discovered [:sequential s/AddonId]]
   [:hot/already-mounted [:sequential s/AddonId]]
   [:hot/injected [:sequential s/AddonId]]
   [:hot/affected [:sequential s/AddonId]]
   [:hot/torn-down [:sequential s/AddonId]]
   [:hot/missing {:optional true} [:map-of :any :any]]
   [:hot/dirs-added [:sequential :string]]
   [:hot/registered [:sequential s/AddonId]]
   [:hot/remembered {:optional true} [:sequential s/AddonId]]
   [:hot/adopted {:optional true} [:sequential s/AddonId]]
   [:hot/deps {:optional true} [:map {:closed false} [:ok? :boolean]]]
   [:teardown/data-preserved? :boolean]
   [:mounted [:sequential ms/MountResult]]
   [:discovery-errors {:optional true} [:sequential :any]]
   [:ok? :boolean]
   [:errors {:optional true} [:sequential :string]]])

(def EjectReport
  "Outcome of plugging addons OUT of a running host
   (hive-addon.hot.inject/eject!) — what was removed, and what STAYS.

   Removed: :hot/torn-down (shut down, reverse order), :hot/unregistered
   (dropped from the host registry through IMountUnregister), :hot/ungoverned
   (no longer governed by the installed lifecycle manager), :hot/unhot
   (deregistered from hive-hot), :hot/dirs-removed (no longer watched),
   :hot/forgotten (dropped from the injected-spec registry).

   Stays — reported, because a caller must not assume it went:
   :hot/unsupported  ids whose host has no IMountUnregister and still holds an
                     inert, shut-down entry.
   :hot/dirs-retained  source dirs still watched: shared with a surviving addon,
                     or hive-hot offers no remove-dirs! (see :hot/dirs-reason).
   :hot/classpath-retained  URLs that stay on the DynamicClassLoader. A
                     java.net.URLClassLoader cannot drop a URL; the code stays
                     loadable until the JVM restarts.
   :hot/namespaces-retained  constructor namespaces still loaded in the image.

   :hot/refused? with :hot/blocking lists active dependents that stopped the
   ejection (pass :cascade? true to take them down and remount them without
   the ejected sibling; they are then under :hot/remounted and :mounted).
   :teardown/data-preserved? is computed over the teardowns that ran. Open."
  [:map {:closed false}
   [:hot/target :any]
   [:hot/ejected [:sequential s/AddonId]]
   [:hot/unknown {:optional true} [:sequential :any]]
   [:hot/refused? {:optional true} :boolean]
   [:hot/blocking {:optional true} [:sequential s/AddonId]]
   [:hot/torn-down [:sequential s/AddonId]]
   [:hot/unregistered [:sequential s/AddonId]]
   [:hot/unsupported [:sequential s/AddonId]]
   [:hot/ungoverned [:sequential s/AddonId]]
   [:hot/unhot [:sequential s/AddonId]]
   [:hot/forgotten [:sequential s/AddonId]]
   [:hot/dirs-removed [:sequential :string]]
   [:hot/dirs-retained [:sequential :string]]
   [:hot/dirs-reason {:optional true} :string]
   [:hot/classpath-retained [:sequential :string]]
   [:hot/namespaces-retained [:sequential :string]]
   [:hot/remounted {:optional true} [:sequential s/AddonId]]
   [:mounted {:optional true} [:sequential ms/MountResult]]
   [:teardown/data-preserved? :boolean]
   [:ok? :boolean]
   [:errors {:optional true} [:sequential :string]]])

;; =============================================================================
;; Port-layer and pure-stratum shapes
;; =============================================================================

(def AddonIdSet
  "A set of addon ids — a reload's seed set, and the closure it produces."
  [:set s/AddonId])

(def MountSpecs
  "The mounted manifest slice a reload reasons over."
  [:sequential ms/MountSpec])

(def DependentsArgs
  "Arglist of hive-addon.hot.cascade/dependents: the specs, then the seed ids.
   A :cat schema, so a schema-driven test applies the subject rather than
   passing the pair as one argument."
  [:cat MountSpecs AddonIdSet])

(def TeardownOutcome
  "What hive-addon.hot.port/IMountDriver's -teardown! reports."
  [:map {:closed false}
   [:torn-down [:sequential s/AddonId]]
   [:errors {:optional true} [:sequential :string]]])

(def NsReloadOutcome
  "What hive-addon.hot.port/INsReloader's -reload-nss! reports. :failed names the
   namespace a reload stopped at; its absence means every namespace loaded.
   A scoped reloader may add :skipped / :dragged (namespaces changed outside
   its roots, declined / loaded as dependents), :unchanged? and :multi-file."
  [:map {:closed false}
   [:loaded [:sequential :any]]
   [:failed {:optional true} [:maybe :any]]
   [:error {:optional true} [:maybe :string]]
   [:skipped {:optional true} [:sequential :any]]
   [:dragged {:optional true} [:sequential :any]]
   [:unchanged? {:optional true} :boolean]
   [:multi-file {:optional true} [:map-of :any [:sequential :any]]]])

;; =============================================================================
;; Local composite registry — mount.schema registry + :hot/* schemas
;; =============================================================================

(def RemountOutcomeArgs
  "Arglist of hive-addon.hot.report/remount-outcome: the ids a remount tore
   down, then the MountReport that followed."
  [:cat [:sequential s/AddonId] ms/MountReport])

(def RemountOutcome
  "What hive-addon.hot.report/remount-outcome answers: which torn-down ids are
   back on their PREVIOUS instance, which are down, and — only when something
   failed — whether everything is running again."
  [:map {:closed true}
   [:hot/restored [:sequential s/AddonId]]
   [:hot/down [:sequential s/AddonId]]
   [:hot/restored? {:optional true} :boolean]])

(def ^:private hot-schemas
  "Static :hot/* -> schema map seeded into the local registry."
  {:hot/trigger            HotTrigger
   :hot/strategy-id        StrategyId
   :hot/source-kind        SourceKind
   :hot/source             AddonSource
   :hot/registration       HotRegistration
   :hot/report             HotReport
   :hot/remount-report     RemountReport
   :hot/inject-report      InjectReport
   :hot/eject-report       EjectReport
   :hot/addon-id-set       AddonIdSet
   :hot/specs              MountSpecs
   :hot/dependents-args    DependentsArgs
   :hot/teardown-outcome   TeardownOutcome
   :hot/ns-reload-outcome  NsReloadOutcome
   :hot/remount-outcome    RemountOutcome})

(def registry
  "Composite malli registry: hive-addon.mount.schema's registry (malli defaults +
   :addon/* + :mount/* schemas) plus this ns's :hot/* schemas. NOT installed as
   the global default — reach it via the wrappers below or {:registry registry}."
  (mr/composite-registry
   ms/registry
   (mr/registry hot-schemas)))

(defn schema
  "Compile ?s against the local :hot/* + :mount/* + :addon/* registry."
  [?s]
  (m/schema ?s {:registry registry}))

(defn validate
  "Registry-aware validate — true/false."
  [?s x]
  (m/validate ?s x {:registry registry}))

(defn explain
  "Registry-aware explain — nil on success, error map on failure."
  [?s x]
  (m/explain ?s x {:registry registry}))

(defn humanize-errors
  "Human-readable error data for x against ?s, or nil if x conforms."
  [?s x]
  (some-> (explain ?s x) me/humanize))

(defn validate*
  "Validate x against ?s, bridging to hive-dsl Result.
   (r/ok x) on success; (r/err category {:explanation <humanized>}) on failure.
   `category` defaults to :hot/schema-violation and must be a qualified keyword
   (hive-dsl taxonomy convention)."
  ([?s x] (validate* ?s x :hot/schema-violation))
  ([?s x category]
   (if (validate ?s x)
     (r/ok x)
     (r/err category {:explanation (humanize-errors ?s x)}))))
