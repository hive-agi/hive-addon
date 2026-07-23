(ns hive-addon.mount.schema
  "Malli value objects for the addon mounter.

   Declarative mount manifests (MountSpec), the pure solver output (MountPlan),
   and the effectful outcome reports (MountResult, MountReport, TeardownReport).
   Shapes are uncompiled malli DATA (house idiom: PascalCase defs) seeded into a
   LOCAL composite registry that COMPOSES hive-addon.schema's registry — the
   AddonId/AddonType/CapabilitySet value objects are reused, never redefined.
   The registry is NEVER installed as the malli global default; reach it via
   `schema`/`validate`/`explain`/`validate*` or by passing {:registry registry}
   yourself.

   Mount shapes are registered under :mount/* keys (:mount/spec, :mount/plan,
   :mount/result, :mount/report, :mount/teardown-report).

   TeardownReport carries the no-nuke invariant as data: :teardown/data-preserved?
   is [:= true], so a report can only validate when teardown preserved data."
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr]
            [hive-addon.schema :as s]
            [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Value-object schemas — uncompiled malli DATA (house idiom: PascalCase defs)
;; =============================================================================

(def InitRetryPolicy
  "Bounded initializer retry policy. :max-attempts includes the first call."
  [:map {:closed false}
   [:max-attempts {:optional true} [:int {:min 1}]]
   [:initial-delay-ms {:optional true} [:int {:min 0}]]
   [:max-delay-ms {:optional true} [:int {:min 0}]]
   [:backoff-factor {:optional true}
    [:or [:int {:min 1}] [:double {:min 1.0}]]]])

(def MountSpec
  "The declarative mount manifest value object — a data description of an addon
   to mount (identity, constructor coordinates, deps, capabilities). Reuses
   hive-addon.schema value objects for :addon/id, :addon/type, :addon/capabilities.
   Open."
  [:map {:closed false}
   [:addon/id s/AddonId]
   [:addon/type s/AddonType]
   [:addon/init-ns [:string {:min 1}]]
   [:addon/init-fn [:string {:min 1}]]
   [:addon/kind {:optional true} [:enum :addon :library]]
   [:addon/version {:optional true} [:string {:min 1}]]
   [:addon/config {:optional true :default {}} [:map-of :keyword :any]]
   [:addon/capabilities {:optional true :default #{}} s/CapabilitySet]
   [:addon/dependencies {:optional true :default #{}} [:set s/AddonId]]
   [:addon/requires-capabilities {:optional true :default #{}} [:set :keyword]]
   [:addon/init-retry {:optional true} InitRetryPolicy]
   [:addon/description {:optional true} [:maybe :string]]
   [:addon/author {:optional true} [:maybe :string]]
   [:addon/license {:optional true} [:maybe :string]]])

(def MountPlan
  "Pure output of solve — the ordered mount plan plus diagnostics as data. No
   IO, no resolved constructors. :ordered is the deterministic topo order (deps
   before dependents); :cycles/:missing/:unmet-capabilities/:duplicates record
   the graceful diagnostics (:duplicates maps an :addon/id shared by >1 spec to
   its count; one surviving spec still appears in :ordered). Open."
  [:map {:closed false}
   [:ordered [:sequential MountSpec]]
   [:cycles [:set s/AddonId]]
   [:missing [:map-of s/AddonId [:set s/AddonId]]]
   [:unmet-capabilities [:map-of s/AddonId [:set :keyword]]]
   [:duplicates [:map-of s/AddonId :int]]])

(def MountResult
  "Per-addon mount outcome. :phase records how far the addon got; :success?
   whether that addon mounted; :errors the accumulated failure strings. Open."
  [:map {:closed false}
   [:addon/id s/AddonId]
   [:success? :boolean]
   [:phase [:enum :config :resolved :registered :initialized :skipped :failed]]
   [:errors {:optional true} [:sequential :string]]
   [:init-attempts {:optional true} [:int {:min 1}]]
   [:already-initialized? {:optional true} :boolean]])

(def MountReport
  "Aggregate outcome of mounting a plan. :ok? is true only when every attempted
   spec succeeded (graceful degrade still yields a report). Open."
  [:map {:closed false}
   [:mounted [:sequential MountResult]]
   [:order [:sequential s/AddonId]]
   [:skipped [:set s/AddonId]]
   [:ok? :boolean]])

(def TeardownReport
  "Outcome of tearing down a mounted system. :teardown/data-preserved? is the
   [:= true] no-nuke invariant — teardown MUST preserve data, so a report can
   only validate when the flag is true. Open."
  [:map {:closed false}
   [:torn-down [:sequential s/AddonId]]
   [:teardown/data-preserved? [:= true]]
   [:errors {:optional true} [:sequential :string]]])

;; =============================================================================
;; Local composite registry — hive-addon.schema registry + :mount/* schemas
;; =============================================================================

(def ^:private mount-schemas
  "Static :mount/* -> schema map seeded into the local registry."
  {:mount/init-retry-policy InitRetryPolicy
   :mount/spec            MountSpec
   :mount/plan            MountPlan
   :mount/result          MountResult
   :mount/report          MountReport
   :mount/teardown-report TeardownReport})

(def registry
  "Composite malli registry: hive-addon.schema's registry (malli defaults +
   :addon/* schemas) plus this ns's :mount/* schemas. NOT installed as the
   global default — reach it via the wrappers below or {:registry registry}."
  (mr/composite-registry
   s/registry
   (mr/registry mount-schemas)))

(defn schema
  "Compile ?s against the local :mount/* + :addon/* registry."
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
   `category` defaults to :mount/schema-violation and must be a qualified
   keyword (hive-dsl taxonomy convention)."
  ([?s x] (validate* ?s x :mount/schema-violation))
  ([?s x category]
   (if (validate ?s x)
     (r/ok x)
     (r/err category {:explanation (humanize-errors ?s x)}))))
