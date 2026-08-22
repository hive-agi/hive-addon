(ns hive-addon.schema
  "Malli schema layer for the IAddon contract.

   Every data shape that crosses the IAddon protocol boundary has a schema
   here, so a host can validate an addon's contract outputs and an addon can
   validate the config it receives. Stratified: the schemas sit as pure data
   BELOW the protocol — they read the protocol's own constants (single source,
   no drift) and the protocol never depends on the schemas.

   Self-contained by design (DDD): the addon bounded context owns its schemas.
   Deps are malli + hive-dsl only — NO hive-spi. Schemas are registered in a
   LOCAL composite registry under :addon/* keys; reach them via `schema`,
   `validate`, `explain` (which thread {:registry registry}) or by passing
   {:registry registry} yourself. The registry is NEVER installed as the malli
   global default (shared-JVM safety).

   Errors bridge to hive-dsl Result: the `validate*`/`validate-addon` helpers
   return (r/ok x) on success and (r/err :addon/... {:explanation ...}) on
   failure, with a qualified-keyword error category."
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr]
            [hive-addon.protocol :as proto]
            [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Value-object schemas — uncompiled malli DATA (house idiom: PascalCase defs)
;; =============================================================================

(def AddonId
  "Stable, non-empty string identifier — the registry key for an addon."
  [:string {:min 1}])

(def AddonType
  "The addon's execution type. Derived from proto/valid-addon-types so the
   enum can never drift from the protocol's own constant."
  (into [:enum] proto/valid-addon-types))

(def Capability
  "A single capability keyword. OPEN: the standard set plus custom addon
   capabilities (:vector-search, :llm-routing, ...) are all legal (OCP)."
  :keyword)

(def CapabilitySet
  "Set of capability keywords an addon provides."
  [:set Capability])

(def HealthStatus
  "Health status keyword. Derived from proto/health-statuses (no drift)."
  (into [:enum] proto/health-statuses))

(def HealthReport
  "Return shape of (health addon)."
  [:map {:closed false}
   [:status HealthStatus]
   [:details {:optional true} [:maybe [:map {:closed false}]]]])

(def InitResult
  "Return shape of (initialize! addon config). Open — addons attach
   arbitrary :metadata; :already-initialized? marks the idempotent re-call."
  [:map {:closed false}
   [:success? :boolean]
   [:errors {:optional true} [:sequential :string]]
   [:already-initialized? {:optional true} :boolean]
   [:metadata {:optional true} [:maybe [:map {:closed false}]]]])

(def ToolDef
  "A single MCP tool definition contributed by (tools addon). :inputSchema is
   the JSON-schema-ish map the host forwards; :handler is an arbitrary fn. Open
   — some addons carry extra keys. Only :name is required: :handler/:inputSchema
   are OPTIONAL BY DESIGN because a :mcp-bridge/:external addon's executable
   handler is supplied by the host transport, not the tool-def."
  [:map {:closed false}
   [:name [:string {:min 1}]]
   [:description {:optional true} :string]
   [:inputSchema {:optional true} [:map {:closed false}]]
   [:handler {:optional true} fn?]])

(def Tools
  "Return shape of (tools addon): a sequence of tool-defs (may be empty)."
  [:sequential ToolDef])

(def SchemaExtensions
  "Return shape of (schema-extensions addon). Two idioms coexist in the wild:
   a SEQUENCE of DataScript attribute-defs (each a map — the protocol's original
   contract) OR a MAP of tool-name -> param JSON-schema (the MCP inputSchema-
   extension seam, used by hive-knowledge). Both — and the empty case of each —
   are accepted; the sequential branch is constrained to a seq of MAPS so a
   malformed non-map element is rejected (protocol.cljc: each schema-def is a
   map suitable for DataScript merge)."
  [:or [:sequential :map] [:map-of :any :any]])

(def HookMap
  "Return shape of (hooks addon): namespaced hook-key -> hook value. Keys are
   qualified keywords matching the host ext-key surface (:cu/a, :catchup/wrap,
   :multi/verb, :spawn/opts-overlay, :op-schema/carto). Values are fns OR data
   bundles (e.g. :op-schema/* -> a schema map), so :any."
  [:map-of :qualified-keyword :any])

(def ExcludedTools
  "Return shape of (excluded-tools addon): a set of tool-name strings this
   addon supersedes from other addons (may be empty)."
  [:set [:string {:min 1}]])

(def AddonConfig
  "Config map passed to (initialize! addon config). Open — contents vary by
   addon type; at minimum the host echoes :addon/id and :addon/config."
  [:map {:closed false}])

;; =============================================================================
;; Local composite registry — malli defaults + this lib's :addon/* schemas
;; =============================================================================

(def ^:private addon-schemas
  "Static :addon/* -> schema map seeded into the local registry so consumers
   can reference addon shapes by keyword (:addon/health, :addon/tools, ...)."
  {:addon/id                AddonId
   :addon/type              AddonType
   :addon/capability        Capability
   :addon/capabilities      CapabilitySet
   :addon/health-status     HealthStatus
   :addon/health            HealthReport
   :addon/init-result       InitResult
   :addon/tool-def          ToolDef
   :addon/tools             Tools
   :addon/schema-extensions SchemaExtensions
   :addon/hooks             HookMap
   :addon/excluded-tools    ExcludedTools
   :addon/config            AddonConfig})

(def registry
  "Composite malli registry: malli defaults + this lib's :addon/* schemas.
   NOT installed as the global default (shared-JVM safety) — reach it via the
   wrappers below or pass {:registry registry} explicitly at call sites."
  (mr/composite-registry
   (m/default-schemas)
   (mr/registry addon-schemas)))

(defn schema
  "Compile ?s against the local :addon/* registry."
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

;; =============================================================================
;; hive-dsl Result bridge — the house error convention
;; =============================================================================

(defn validate*
  "Validate x against ?s, bridging to hive-dsl Result.
   (r/ok x) on success; (r/err category {:explanation <humanized>}) on failure.
   `category` defaults to :addon/schema-violation and must be a qualified
   keyword (hive-dsl taxonomy convention)."
  ([?s x] (validate* ?s x :addon/schema-violation))
  ([?s x category]
   (if (validate ?s x)
     (r/ok x)
     (r/err category {:explanation (humanize-errors ?s x)}))))

;; Pure, side-effect-free contract methods -> [schema optional?]. initialize!
;; and shutdown! are excluded (they mutate); validate InitResult separately.
(def ^:private contract-checks
  [[:addon/id                #'proto/addon-id          AddonId          false]
   [:addon/type              #'proto/addon-type        AddonType        false]
   [:addon/capabilities      #'proto/capabilities      CapabilitySet    false]
   [:addon/tools             #'proto/tools             Tools            false]
   [:addon/schema-extensions #'proto/schema-extensions SchemaExtensions false]
   [:addon/health            #'proto/health            HealthReport     false]
   [:addon/excluded-tools    #'proto/excluded-tools    ExcludedTools    true]
   [:addon/hooks             #'proto/hooks             HookMap          true]])

(def ^:private unimplemented-method-re
  "Messages the hosts use for a protocol method that is not implemented.

   Measured 2026-08-22. The JVM alone has TWO phrasings, which is why matching
   one measured example is not enough — the suite caught the second:
     JVM defrecord \"Method p.Partial.b()Ljava/lang/Object; is abstract\"
     JVM reify     \"Receiver class ...$reify__8710 does not define or inherit an
                    implementation of the resolved method 'abstract java.lang.Object
                    excluded_tools()' of interface hive_addon.protocol.IAddon.\"
     JVM extend-*  \"No implementation of method: :b of protocol: ...\"
     cljw          \"No implementation of method 'b' on protocol 'IThing' for type 'Partial'\"
     cljrs         \"runtime error: No implementation of protocol IThing for type Partial\"
   cljs contributes \"no protocol method\" / \"nothing implements\".

   Matching the message rather than the exception CLASS is what makes this
   portable: the class name differs per host and `AbstractMethodError` cannot
   even be named off the JVM."
  #"(?i)is abstract|does not define or inherit an implementation|no implementation of (method|protocol)|no protocol method|nothing implements")

(defn validate-addon
  "Validate a live IAddon instance's contract OUTPUTS against the schemas.
   Exercises only the pure, non-mutating methods (does NOT call initialize! or
   shutdown!). Returns (r/ok addon) when every output conforms, else the FIRST
   (r/err :addon/contract-violation {:method .. :value .. :explanation ..}), or
   (r/err :addon/method-threw ..) if a method blows up for a non-contract reason.

   excluded-tools and hooks are optional per the protocol (legacy addons may
   omit them, defaulting to #{}/{}); an unimplemented optional method is
   skipped, an unimplemented required method is a violation. Non-implementation
   is recognized for BOTH extension mechanisms: inline deftype/defrecord/reify
   omission and extend-*/metadata omission.

   Detection is by MESSAGE, not by exception class. The class differs per host
   and naming one is not portable — `AbstractMethodError` does not exist off the
   JVM, and `#?(:clj ...)` does not select the JVM (cljw presents :clj), so the
   old class-based branch was TAKEN on cljw and died at analysis. Measured
   messages for a missing protocol method:

     JVM   \"Method p.Partial.b()Ljava/lang/Object; is abstract\"
     cljw  \"No implementation of method 'b' on protocol 'IThing' for type ...\"
     cljrs \"runtime error: No implementation of protocol IThing for type ...\"

   `ex-message` reads all of them (it is defined on Throwable on the JVM), so
   the whole check is host-free."
  [addon]
  (letfn [(unimplemented? [t]
            ;; NOTE: a genuine \"is abstract\" / \"no implementation\" error raised
            ;; from *inside* an implemented method body also reads as
            ;; unimplemented here — an accepted edge for the optional-method skip.
            (boolean (some->> (ex-message t) (re-find unimplemented-method-re))))
          (err-msg [t] (ex-message t))]
    (reduce
     (fn [_acc [k getter sch optional?]]
       (let [call (try {:v (getter addon)}
                       ;; TOTAL reader conditional — :clj for the JVM and cljw
                       ;; (both have Throwable), :default for cljs and cljrs. A
                       ;; non-total #? would vanish on cljrs and stop catching.
                       (catch #?(:clj Throwable :default :default) t
                         (if (unimplemented? t) ::unimplemented {:throw t})))]
         (cond
           (and optional? (= call ::unimplemented)) (r/ok addon)
           (= call ::unimplemented)
           (reduced (r/err :addon/contract-violation
                           {:method k :explanation ["required protocol method not implemented"]}))
           (:throw call)
           (reduced (r/err :addon/method-threw
                           {:method k :explanation [(err-msg (:throw call))]}))
           (validate sch (:v call)) (r/ok addon)
           :else
           (reduced (r/err :addon/contract-violation
                           {:method k :value (:v call)
                            :explanation (humanize-errors sch (:v call))})))))
     (r/ok addon)
     contract-checks)))

;; =============================================================================
;; Function-schema DATA for the 10 IAddon methods (scoped/reversible audit)
;; =============================================================================

(def method-schemas
  "Malli function schemas ([:=> ...]) for the IAddon methods, as DATA keyed by
   fully-qualified method symbol. For scoped, reversible instrumentation only
   (malli.instrument over a select-keys slice) — NEVER attach globally or call
   bare malli.instrument/instrument!: that mutates var roots across the shared
   JVM. `this` is :any (the addon instance)."
  {'hive-addon.protocol/addon-id          [:=> [:cat :any] AddonId]
   'hive-addon.protocol/addon-type        [:=> [:cat :any] AddonType]
   'hive-addon.protocol/capabilities      [:=> [:cat :any] CapabilitySet]
   'hive-addon.protocol/initialize!       [:=> [:cat :any AddonConfig] InitResult]
   'hive-addon.protocol/shutdown!         [:=> [:cat :any] :nil]
   'hive-addon.protocol/tools             [:=> [:cat :any] Tools]
   'hive-addon.protocol/schema-extensions [:=> [:cat :any] SchemaExtensions]
   'hive-addon.protocol/health            [:=> [:cat :any] HealthReport]
   'hive-addon.protocol/excluded-tools    [:=> [:cat :any] ExcludedTools]
   'hive-addon.protocol/hooks             [:=> [:cat :any] HookMap]})
