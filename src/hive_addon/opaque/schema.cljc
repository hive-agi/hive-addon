(ns hive-addon.opaque.schema
  "Malli value objects for the OPAQUE addon wire — the contract between a host
   proxying an IAddon and the out-of-process kernel that really implements it.

   Shapes are uncompiled malli DATA (house idiom: PascalCase defs) seeded into a
   LOCAL composite registry that COMPOSES hive-addon.schema's registry — the
   AddonId/AddonType/CapabilitySet value objects are reused, never redefined.
   The registry is NEVER installed as the malli global default; reach it via
   `schema`/`validate`/`explain`/`validate*` or by passing {:registry registry}.

   Opaque shapes are registered under :opaque/* keys (:opaque/wire,
   :opaque/request, :opaque/response, :opaque/describe, :opaque/spec).

   The kernel side never loads this namespace: hive-addon.opaque.codec and
   hive-addon.opaque.serve are malli-free so a cljw-built binary carries no
   schema runtime. These schemas are the host-side single source the m/=>
   contracts (hive-addon.opaque.contracts) and the generated tests read."
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr]
            [hive-addon.schema :as s]
            [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Wire values — what one EDN line may carry across the process boundary
;; =============================================================================

(def WireVal
  "A value the wire carries, recursively: the JSON-ish subset MCP hands a tool
   (nil, string, int, double, boolean, vector, map) plus keywords, symbols and
   sets, so a param map, a tool result, a health report, a capability set and an
   addon config all fit. Doubles exclude NaN and the infinities, which do not
   round-trip through equality.

   This schema and hive-addon.opaque.codec/safe? are the SAME judgement stated
   twice, once for the host and once for the kernel, because the kernel cannot
   load malli. `edn-safe` is contracted to return a WireVal
   (hive-addon.opaque.contracts), which is where the two are held together: a
   value codec admits and this schema rejects fails that contract."
  [:schema {:registry {::wire [:or
                               :nil :string :int
                               [:double {:gen/infinite? false :gen/NaN? false}]
                               :boolean :keyword :symbol
                               [:vector [:ref ::wire]]
                               [:set [:ref ::wire]]
                               [:map-of [:or :string :keyword :symbol] [:ref ::wire]]]}}
   ::wire])

(def Op
  "The verbs the proxy sends. CLOSED: the proxy implements IAddon, whose method
   set is closed, so the op set is closed with it (cardinality decides)."
  [:enum :addon/describe :addon/initialize! :addon/shutdown! :addon/health
   :addon/tool :addon/hook])

(def Request
  "One request line: the op and its EDN-safe argument."
  [:map {:closed true}
   [:op Op]
   [:args {:optional true} WireVal]])

(def Response
  "One response line: the op echoed with its result, or an error string. An
   error may omit :op — a line that failed to parse has no op to echo."
  [:or
   [:map {:closed true} [:op Op] [:result WireVal]]
   [:map {:closed true} [:op {:optional true} :keyword] [:error :string]]])

;; =============================================================================
;; What the kernel says about itself — the IAddon surface as data
;; =============================================================================

(def ToolSummary
  "An IAddon tool-def with its :handler removed: everything but the fn crosses
   the wire, and the proxy supplies a handler that calls back."
  [:map {:closed false}
   [:name [:string {:min 1}]]
   [:description {:optional true} :string]
   [:inputSchema {:optional true} [:map {:closed false}]]])

(def HookSummary
  "How one hook crosses the wire. A fn-valued hook is announced as :fn and the
   proxy installs a fn that calls back; a data-valued hook travels as its value."
  [:or
   [:map {:closed true} [:kind [:= :fn]]]
   [:map {:closed true} [:kind [:= :data]] [:value WireVal]]])

(def Describe
  "The :addon/describe result — the whole pure IAddon surface, as data."
  [:map {:closed false}
   [:addon/id s/AddonId]
   [:addon/type s/AddonType]
   [:addon/capabilities s/CapabilitySet]
   [:addon/tools [:sequential ToolSummary]]
   [:addon/excluded-tools s/ExcludedTools]
   [:addon/schema-extensions s/SchemaExtensions]
   [:addon/hooks [:map-of :qualified-keyword HookSummary]]])

;; =============================================================================
;; What the host needs to reach the kernel
;; =============================================================================

(def OpaqueSpec
  "Everything the proxy needs to reach one opaque kernel: the executable and its
   argv. :opaque/id is the addon id the manifest promised; when present the
   kernel's own describe must agree. :opaque/capabilities is what the manifest
   advertises before the kernel is running. Rides as :addon/config of the mount
   manifest."
  [:map {:closed false}
   [:opaque/exec [:string {:min 1}]]
   [:opaque/args {:optional true} [:vector :string]]
   [:opaque/id {:optional true} s/AddonId]
   [:opaque/capabilities {:optional true} s/CapabilitySet]])

;; =============================================================================
;; Local composite registry — hive-addon.schema registry + :opaque/* schemas
;; =============================================================================

(def ^:private opaque-schemas
  "Static :opaque/* -> schema map seeded into the local registry."
  {:opaque/wire         WireVal
   :opaque/op           Op
   :opaque/request      Request
   :opaque/response     Response
   :opaque/tool-summary ToolSummary
   :opaque/hook-summary HookSummary
   :opaque/describe     Describe
   :opaque/spec         OpaqueSpec})

(def registry
  "Composite malli registry: hive-addon.schema's registry (malli defaults +
   :addon/* schemas) plus this ns's :opaque/* schemas. NOT installed as the
   global default — reach it via the wrappers below or {:registry registry}."
  (mr/composite-registry
   s/registry
   (mr/registry opaque-schemas)))

(defn schema
  "Compile ?s against the local :opaque/* + :addon/* registry."
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
   `category` defaults to :opaque/schema-violation and must be a qualified
   keyword (hive-dsl taxonomy convention)."
  ([?s x] (validate* ?s x :opaque/schema-violation))
  ([?s x category]
   (if (validate ?s x)
     (r/ok x)
     (r/err category {:explanation (humanize-errors ?s x)}))))
