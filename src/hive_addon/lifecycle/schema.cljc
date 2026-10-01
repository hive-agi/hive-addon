(ns hive-addon.lifecycle.schema
  "Malli value objects for the addon lifecycle: when an addon is mounted, when it
   is let go, and what stands in for it while it is not mounted.

   Shapes are uncompiled malli DATA seeded into a LOCAL composite registry over
   hive-addon.mount.schema's; never installed as the global default.

   Registered keys: :lifecycle/policy :lifecycle/decl :lifecycle/surface
   :lifecycle/use-state :lifecycle/sweep-plan :lifecycle/activation-report
   :lifecycle/eviction-report :lifecycle/boot-report."
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
;; Value objects
;; =============================================================================

(def Policy
  ":eager   mounted at boot, evicted only on request.
   :lazy    dormant until first use, evicted after :idle-ms without use.
   :pinned  mounted at boot, never evicted; its parts still are."
  [:enum :eager :lazy :pinned])

(def IdleMs
  "Milliseconds without use before a :lazy addon (or a part) is evictable."
  [:int {:min 1}])

(def LifecycleDecl
  "What a manifest's :addon/lifecycle, or a host override, declares. Open."
  [:map {:closed false}
   [:policy {:optional true} Policy]
   [:idle-ms {:optional true} IdleMs]])

(def Lifecycle
  "A resolved lifecycle: every key present."
  [:map {:closed false}
   [:policy Policy]
   [:idle-ms IdleMs]])

(def ToolDecl
  "A tool an addon contributes, as advertised: an MCP tool-def with no handler."
  [:map {:closed false}
   [:name [:string {:min 1}]]])

(def Surface
  "Everything a caller can reach an addon through, known WITHOUT mounting it.
   :tools    top-level tool-defs.
   :commands {host-tool-name [command-name ...]} contributed into host tools.
   :commands may name the full dispatch token (\"carto\") or a leaf; the host
   decides how a stub routes."
  [:map {:closed false}
   [:tools {:optional true :default []} [:sequential ToolDecl]]
   [:commands {:optional true :default {}}
    [:map-of [:string {:min 1}] [:sequential [:string {:min 1}]]]]])

(def Phase
  [:enum :dormant :activating :active :evicting :failed])

(def UseState
  "Per-addon runtime state the lifecycle keeps."
  [:map {:closed false}
   [:addon/id s/AddonId]
   [:phase Phase]
   [:lifecycle Lifecycle]
   [:last-used-ms [:maybe :int]]
   [:in-flight [:int {:min 0}]]
   [:activations [:int {:min 0}]]
   [:evictions [:int {:min 0}]]
   [:surface/source {:optional true} [:enum :declared :learned :none]]
   [:last-error {:optional true} [:maybe :string]]])

(def KeepReason
  "Why a sweep left an addon mounted."
  [:enum :pinned :eager :not-active :in-flight :dependent-active :fresh :never-used])

(def SweepPlan
  ":evict is ordered dependents-first, so evicting in order never pulls an
   instance out from under a live dependent."
  [:map {:closed false}
   [:now-ms :int]
   [:evict [:sequential s/AddonId]]
   [:kept [:map-of s/AddonId KeepReason]]])

(def ActivationReport
  [:map {:closed false}
   [:addon/id s/AddonId]
   [:ok? :boolean]
   [:activated [:sequential s/AddonId]]
   [:already-active? {:optional true} :boolean]
   [:mounted {:optional true} [:sequential ms/MountResult]]
   [:ns-reloaded {:optional true} [:sequential :string]]
   [:errors {:optional true} [:sequential :string]]])

(def EvictionReport
  [:map {:closed false}
   [:addon/id s/AddonId]
   [:ok? :boolean]
   [:evicted? :boolean]
   [:reason {:optional true} [:or KeepReason :keyword]]
   [:parts-closed {:optional true} [:sequential :keyword]]
   [:teardown/data-preserved? [:= true]]
   [:errors {:optional true} [:sequential :string]]])

(def BootReport
  [:map {:closed false}
   [:eager [:sequential s/AddonId]]
   [:dormant [:sequential s/AddonId]]
   [:downgraded [:map-of s/AddonId :keyword]]
   [:mount {:optional true} ms/MountReport]
   [:ok? :boolean]])

;; =============================================================================
;; Local registry
;; =============================================================================

(def ^:private lifecycle-schemas
  {:lifecycle/policy            Policy
   :lifecycle/decl              LifecycleDecl
   :lifecycle/resolved          Lifecycle
   :lifecycle/surface           Surface
   :lifecycle/use-state         UseState
   :lifecycle/sweep-plan        SweepPlan
   :lifecycle/activation-report ActivationReport
   :lifecycle/eviction-report   EvictionReport
   :lifecycle/boot-report       BootReport})

(def registry
  (mr/composite-registry ms/registry (mr/registry lifecycle-schemas)))

(defn validate [?s x] (m/validate ?s x {:registry registry}))

(defn explain [?s x] (m/explain ?s x {:registry registry}))

(defn humanize-errors [?s x] (some-> (explain ?s x) me/humanize))

(defn validate*
  "(r/ok x) or (r/err category {:explanation ...})."
  ([?s x] (validate* ?s x :lifecycle/schema-violation))
  ([?s x category]
   (if (validate ?s x)
     (r/ok x)
     (r/err category {:explanation (humanize-errors ?s x)}))))
