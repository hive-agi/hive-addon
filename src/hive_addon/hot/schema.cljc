(ns hive-addon.hot.schema
  "Malli value objects for the IAddon hot-reload bridge.

   Shapes are uncompiled malli DATA (house idiom: PascalCase defs) seeded into a
   LOCAL composite registry that COMPOSES hive-addon.mount.schema's registry —
   AddonId/MountResult/MountSpec are reused, never redefined. The registry is
   NEVER installed as the malli global default; reach it via
   `schema`/`validate`/`explain`/`validate*` or by passing {:registry registry}.

   Hot shapes are registered under :hot/* keys (:hot/registration, :hot/report,
   :hot/remount-report, :hot/source, :hot/strategy-id).

   Two invariants are carried as DATA rather than prose:
   - RemountReport's :teardown/data-preserved? is [:= true] — the no-nuke
     invariant inherited from TeardownReport.
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

   :hot/strategy names the strategy that ran. :teardown/data-preserved? inherits
   the no-nuke invariant: [:= true]. Open."
  [:map {:closed false}
   [:hot/trigger HotTrigger]
   [:hot/strategy StrategyId]
   [:hot/changed-ns {:optional true} [:maybe [:string {:min 1}]]]
   [:hot/seeds [:set s/AddonId]]
   [:hot/affected [:sequential s/AddonId]]
   [:hot/torn-down [:sequential s/AddonId]]
   [:hot/cycles {:optional true} [:set s/AddonId]]
   [:hot/ns-reloaded {:optional true} [:sequential :string]]
   [:teardown/data-preserved? [:= true]]
   [:mounted [:sequential ms/MountResult]]
   [:ok? :boolean]
   [:errors {:optional true} [:sequential :string]]])

;; =============================================================================
;; Local composite registry — mount.schema registry + :hot/* schemas
;; =============================================================================

(def ^:private hot-schemas
  "Static :hot/* -> schema map seeded into the local registry."
  {:hot/trigger        HotTrigger
   :hot/strategy-id    StrategyId
   :hot/source-kind    SourceKind
   :hot/source         AddonSource
   :hot/registration   HotRegistration
   :hot/report         HotReport
   :hot/remount-report RemountReport})

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
