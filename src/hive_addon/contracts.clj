(ns hive-addon.contracts
  "The m/=> contract spine for the PORTABLE stratum: the namespaces that must
   load on the JVM, cljw and cljrs (hive-addon.mount.portable-test derives
   them), plus the malli-free pure namespaces beside them.

   Every contract lives HERE rather than beside its function for the reason
   hive-addon.opaque.contracts gives for the kernel tier: those namespaces are
   malli-free by admission rule. malli does not load on cljrs (malli.impl.regex
   needs `deftype`) and the portable oracle requires them on all three hosts,
   so a `(m/=> ...)` next to `registry.tools/register!` would end the three-host
   claim. Stating the contracts from outside keeps the stratum lean and the
   host fully checked.

   Contracts name their subject by its FULLY QUALIFIED symbol. `m/=>` registers
   under the namespace part of the symbol as written, so an alias here would
   register the schema under the alias and no instrumentation would reach it.

   HOST-ONLY, and loading it is OPTIONAL. Nothing requires this namespace at
   runtime; the suite loads it (hive-addon.contract-coverage-test) and a host
   that wants instrumentation may too.

   Schemas are referenced as VARS, never registry keywords, so a contract
   resolves without anyone having installed a registry."
  (:require [hive-addon.cli]
            [hive-addon.cli.response]
            [hive-addon.cli.tree]
            [hive-addon.hot.cascade]
            [hive-addon.hot.report]
            [hive-addon.hot.schema :as hs]
            [hive-addon.lifecycle.oracle]
            [hive-addon.lifecycle.policy]
            [hive-addon.lifecycle.port :as lport]
            [hive-addon.lifecycle.schema :as ls]
            [hive-addon.lifecycle.surface]
            [hive-addon.mount.injected :as injected]
            [hive-addon.mount.port :as mport]
            [hive-addon.mount.schema :as ms]
            [hive-addon.mount.solve]
            [hive-addon.plug.lint]
            [hive-addon.plug.merge]
            [hive-addon.plug.schema :as ps]
            [hive-addon.plug.source :as source]
            [hive-addon.protocol]
            [hive-addon.registry.commands]
            [hive-addon.registry.extension]
            [hive-addon.registry.schema]
            [hive-addon.registry.tools]
            [hive-addon.wire]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Shared shapes
;; =============================================================================

(def AnyMap [:map-of :any :any])

(def Spec
  "A spec as the pure derivations read it: any map. They default every key
   they read, so narrowing to MountSpec would delete the defaulting branches
   from the domain."
  AnyMap)

(def Specs
  "A spec collection. Callers pass vectors and SETS alike (solve folds its
   input into a set before it reaches edges), so both are the domain."
  [:or [:sequential Spec] [:set Spec]])

(def Ids
  "A collection of addon ids, as seeds or closures: a set or a sequence."
  [:or [:set :any] [:sequential :any]])

(def ToolDefIn
  "What registry.tools accepts: its own precondition, :name string and an
   invokable :handler."
  [:map {:closed false} [:name :string] [:handler ifn?]])

(def CommandSpec
  "One contributed command as registry.commands stores it."
  [:map {:closed false} [:addon :any]])

(def CliResponse
  [:map {:closed true} [:type [:= "text"]] [:text :any] [:isError {:optional true} [:= true]]])

(defn- satisfies-schema
  "A predicate schema for protocol satisfaction. Generation is not offered:
   an instance is constructed by its own factory, never drawn."
  [proto label]
  [:fn {:error/message (str "should satisfy " label)} #(satisfies? proto %)])

;; =============================================================================
;; registry.*: atom-backed registries (contract the arguments they accept and
;; the shapes they answer; the atom itself is the namespace's private state)
;; =============================================================================

(m/=> hive-addon.registry.tools/register! [:=> [:cat ToolDefIn] :string])
(m/=> hive-addon.registry.tools/registered [:=> [:cat] [:vector ToolDefIn]])
(m/=> hive-addon.registry.tools/registered-names [:=> [:cat] [:vector :string]])
(m/=> hive-addon.registry.tools/get-tool [:=> [:cat :any] [:maybe ToolDefIn]])
;; deregister! echoes its argument whatever it is: the identity is the contract.
(m/=> hive-addon.registry.tools/deregister! [:=> [:cat :any] :any])
(m/=> hive-addon.registry.tools/clear! [:=> [:cat] :nil])

(m/=> hive-addon.registry.extension/register! [:=> [:cat :keyword ifn?] :keyword])
(m/=> hive-addon.registry.extension/register-many!
      [:=> [:cat [:map-of :keyword ifn?]] [:maybe [:sequential :keyword]]])
(m/=> hive-addon.registry.extension/get-extension
      [:function [:=> [:cat :any] [:maybe ifn?]] [:=> [:cat :any :any] :any]])
(m/=> hive-addon.registry.extension/extension-available? [:=> [:cat :any] :boolean])
(m/=> hive-addon.registry.extension/registered-keys [:=> [:cat] [:set :keyword]])
(m/=> hive-addon.registry.extension/deregister! [:=> [:cat :any] :any])
(m/=> hive-addon.registry.extension/clear! [:=> [:cat] :nil])

(m/=> hive-addon.registry.schema/register! [:=> [:cat :string AnyMap] :string])
(m/=> hive-addon.registry.schema/get-extensions [:=> [:cat :any] [:maybe AnyMap]])
(m/=> hive-addon.registry.schema/extended-tool-names [:=> [:cat] [:vector :string]])
(m/=> hive-addon.registry.schema/clear! [:=> [:cat] :nil])

(m/=> hive-addon.registry.commands/add-listener! [:=> [:cat :any ifn?] :any])
(m/=> hive-addon.registry.commands/remove-listener! [:=> [:cat :any] :any])
(m/=> hive-addon.registry.commands/listener-ids [:=> [:cat] [:vector :any]])
(m/=> hive-addon.registry.commands/contribute!
      [:=> [:cat :any :any [:map-of [:or :keyword :string :symbol] AnyMap]] [:vector :string]])
(m/=> hive-addon.registry.commands/retract! [:=> [:cat :any :any] :nil])
(m/=> hive-addon.registry.commands/retract-all! [:=> [:cat :any] :nil])
(m/=> hive-addon.registry.commands/get-commands [:=> [:cat :any] [:maybe [:map-of :string CommandSpec]]])
(m/=> hive-addon.registry.commands/contributed-tool-names [:=> [:cat] [:vector :any]])
(m/=> hive-addon.registry.commands/clear! [:=> [:cat] :nil])

;; =============================================================================
;; cli.*: command parsing and dispatch
;; =============================================================================

(m/=> hive-addon.cli.response/text [:=> [:cat :any] CliResponse])
(m/=> hive-addon.cli.response/error [:=> [:cat :any] CliResponse])
(m/=> hive-addon.cli.response/error? [:=> [:cat :any] :boolean])

(m/=> hive-addon.cli.tree/normalize-command [:=> [:cat :any] [:maybe :string]])
(m/=> hive-addon.cli.tree/parse-command [:=> [:cat [:maybe :string]] [:maybe [:vector {:min 1} :keyword]]])
(m/=> hive-addon.cli.tree/collect-command-paths [:=> [:cat AnyMap [:vector :any]] [:vector [:vector :any]]])
(m/=> hive-addon.cli.tree/format-help [:=> [:cat AnyMap] :string])
(m/=> hive-addon.cli.tree/resolve-handler
      [:=> [:cat :any [:sequential :any]]
       [:or
        [:map {:closed true} [:handler ifn?] [:path-used [:vector :any]] [:remaining [:vector :any]]]
        [:map {:closed true} [:tree :any] [:path-used [:vector :any]]]
        [:map {:closed true} [:error [:= :not-found]] [:path-used [:vector :any]] [:remaining [:vector :any]]]]])

(m/=> hive-addon.cli/make-handler
      [:function
       [:=> [:cat AnyMap] ifn?]
       [:=> [:cat AnyMap [:maybe [:map {:closed false}
                                  [:coerce-schema {:optional true} :any]
                                  [:coerce-fn {:optional true} [:maybe ifn?]]
                                  [:error-fn {:optional true} [:maybe ifn?]]]]]
        ifn?]])

;; =============================================================================
;; plug.*: config merge, lint, source classification
;; =============================================================================

;; deep-merge folds any values: a non-map right side REPLACES, by design.
(m/=> hive-addon.plug.merge/deep-merge [:=> [:cat [:* :any]] :any])
(m/=> hive-addon.plug.merge/merge-configs [:=> [:cat [:sequential [:maybe AnyMap]]] AnyMap])
;; check's input is any map, not IaddonConfig: lint runs on what a user wrote,
;; and it is the lint (not the schema) that must name a bad repo or credential.
;; Its :ok echoes that map back unchanged.
(m/=> hive-addon.plug.lint/check
      [:=> [:cat AnyMap]
       [:or [:map {:closed true} [:ok AnyMap]]
            [:map {:closed true} [:error [:= :iaddon/lint-failed]] [:violations ps/Violations]]]])
;; A plug with no :source hands nil to both classifiers; nil is no family.
(m/=> hive-addon.plug.source/coord->source
      [:=> [:cat [:maybe AnyMap]] [:maybe (satisfies-schema source/ISource "ISource")]])
(m/=> hive-addon.plug.source/families [:=> [:cat [:maybe AnyMap]] [:set [:enum :local :git :mvn]]])

;; =============================================================================
;; mount.solve / mount.port / mount.injected
;; =============================================================================

(m/=> hive-addon.mount.solve/edges
      [:function
       [:=> [:cat Specs] [:set [:tuple :any :any]]]
       [:=> [:cat Specs [:sequential :any]] [:set [:tuple :any :any]]]])

(def SolvePlan
  "solve's plan, stated over the specs it was GIVEN: :ordered holds those very
   maps. ms/MountPlan narrows :ordered to full MountSpecs, which is right for
   the mounter and wrong for hive-addon.composite, which solves projected
   member specs (id, dependencies, capabilities only)."
  [:map {:closed true}
   [:ordered [:vector AnyMap]]
   [:cycles [:set :any]]
   [:missing [:map-of :any [:set :any]]]
   [:unmet-capabilities [:map-of :any [:set :any]]]
   [:duplicates [:map-of :any pos-int?]]])

(m/=> hive-addon.mount.solve/solve
      [:function
       [:=> [:cat Specs] SolvePlan]
       [:=> [:cat Specs [:maybe AnyMap]]
        [:or SolvePlan
             [:map {:closed true} [:error [:= :mount/unsolvable]] [:cycles [:set :any]]]]]])

(m/=> hive-addon.mount.port/resolve-config-default [:=> [:cat AnyMap] :any])
(m/=> hive-addon.mount.port/atom-mount-host [:=> [:cat] (satisfies-schema mport/IMountHost "IMountHost")])

(m/=> hive-addon.mount.injected/union-specs [:=> [:cat [:sequential AnyMap] [:sequential AnyMap]] [:vector AnyMap]])
(m/=> hive-addon.mount.injected/entries-for-path [:=> [:cat [:sequential AnyMap] :any] [:vector AnyMap]])
(m/=> hive-addon.mount.injected/remember! [:=> [:cat [:sequential injected/InjectedEntry]] [:vector :string]])
(m/=> hive-addon.mount.injected/forget! [:=> [:cat Ids] [:vector :any]])
(m/=> hive-addon.mount.injected/entries [:=> [:cat] [:vector injected/InjectedEntry]])
(m/=> hive-addon.mount.injected/entry [:=> [:cat :any] [:maybe injected/InjectedEntry]])
(m/=> hive-addon.mount.injected/injected-specs [:=> [:cat] [:vector AnyMap]])
(m/=> hive-addon.mount.injected/reset-registry! [:=> [:cat] :nil])

;; =============================================================================
;; hot.cascade / hot.report
;; =============================================================================

(m/=> hive-addon.hot.cascade/dependents
      [:function
       [:=> [:cat Specs Ids] [:set :any]]
       [:=> [:cat Specs Ids [:maybe AnyMap]] [:set :any]]])
(m/=> hive-addon.hot.cascade/affected-plan
      [:function
       [:=> [:cat Specs Ids]
        [:map {:closed false} [:ordered [:vector AnyMap]] [:hot/affected-ids [:set :any]]]]
       [:=> [:cat Specs Ids [:maybe AnyMap]] AnyMap]])
(m/=> hive-addon.hot.cascade/ns->addon-ids [:=> [:cat Specs] [:map-of :any [:set :any]]])
(m/=> hive-addon.hot.cascade/seeds-for-ns [:=> [:cat Specs :any] [:set :any]])

(m/=> hive-addon.hot.report/released? [:=> [:cat AnyMap] :boolean])
(m/=> hive-addon.hot.report/data-preserved? [:=> [:cat [:sequential AnyMap]] :boolean])
(m/=> hive-addon.hot.report/remount-outcome [:=> hs/RemountOutcomeArgs hs/RemountOutcome])
(m/=> hive-addon.hot.report/released-dirs [:=> [:cat [:or [:sequential :string] [:set :string]] [:or [:sequential :string] [:set :string]]] [:vector :string]])
(m/=> hive-addon.hot.report/dirs-outcome
      [:=> [:cat [:sequential AnyMap] [:sequential AnyMap]]
       [:map {:closed true}
        [:removed [:vector :any]]
        [:retained [:vector :any]]
        [:shared [:map-of :any [:vector :any]]]
        [:reason {:optional true} :string]]])

;; =============================================================================
;; lifecycle.*: pure policy, surface, oracle, in-memory store
;; =============================================================================

(m/=> hive-addon.lifecycle.surface/tool-decl [:=> [:cat AnyMap] AnyMap])
(m/=> hive-addon.lifecycle.surface/normalize
      [:=> [:cat [:maybe AnyMap]]
       [:map {:closed true}
        [:tools [:vector AnyMap]]
        [:commands [:map-of :string [:vector :string]]]]])
(m/=> hive-addon.lifecycle.surface/blank? [:=> [:cat [:maybe AnyMap]] :boolean])
(m/=> hive-addon.lifecycle.surface/well-formed? [:=> [:cat :any] :boolean])
(m/=> hive-addon.lifecycle.surface/declared [:=> [:cat AnyMap] [:maybe ls/Surface]])
(m/=> hive-addon.lifecycle.surface/observed
      [:=> [:cat :any [:maybe [:sequential AnyMap]] [:maybe AnyMap]] ls/Surface])
(m/=> hive-addon.lifecycle.surface/resolve-surface
      [:=> [:cat AnyMap :any]
       [:or [:tuple ls/Surface [:enum :declared :learned]]
            [:tuple :nil [:= :none]]]])

(m/=> hive-addon.lifecycle.policy/resolve-lifecycle [:=> [:cat AnyMap :any :any] ls/Lifecycle])
(m/=> hive-addon.lifecycle.policy/resolve-all [:=> [:cat Specs [:maybe AnyMap]] [:map-of :any ls/Lifecycle]])
(m/=> hive-addon.lifecycle.policy/dependency-closure [:=> [:cat Specs Ids] [:set :any]])
(m/=> hive-addon.lifecycle.policy/dependent-closure [:=> [:cat Specs Ids] [:set :any]])
(m/=> hive-addon.lifecycle.policy/boot-partition
      [:=> [:cat Specs [:map-of :any AnyMap] ifn?]
       [:map {:closed true}
        [:eager [:vector :any]]
        [:dormant [:vector :any]]
        [:downgraded [:map-of :any [:enum :required-by-eager :no-surface]]]]])
(m/=> hive-addon.lifecycle.policy/activation-ids [:=> [:cat Specs :any ifn?] [:vector :any]])
(m/=> hive-addon.lifecycle.policy/lazy-permitted? [:=> [:cat [:maybe AnyMap]] :boolean])
(m/=> hive-addon.lifecycle.policy/sweep-plan
      [:=> [:cat :int Specs [:map-of :any AnyMap]]
       [:map {:closed true}
        [:now-ms :int]
        [:evict [:vector :any]]
        [:kept [:map-of :any ls/KeepReason]]]])

(m/=> hive-addon.lifecycle.oracle/install-dormancy! [:=> [:cat [:maybe ifn?]] [:maybe ifn?]])
(m/=> hive-addon.lifecycle.oracle/reset-dormancy! [:=> [:cat] :nil])
(m/=> hive-addon.lifecycle.oracle/dormant? [:=> [:cat :any] :boolean])

(m/=> hive-addon.lifecycle.port/atom-surface-store
      [:function
       [:=> [:cat] (satisfies-schema lport/ISurfaceStore "ISurfaceStore")]
       [:=> [:cat AnyMap] (satisfies-schema lport/ISurfaceStore "ISurfaceStore")]])

;; =============================================================================
;; protocol predicates, wire
;; =============================================================================

(m/=> hive-addon.protocol/addon? [:=> [:cat :any] :boolean])
(m/=> hive-addon.protocol/valid-addon-type? [:=> [:cat :any] :boolean])
(m/=> hive-addon.protocol/unimplemented-method? [:=> [:cat :any] :boolean])
(m/=> hive-addon.protocol/healthy? [:=> [:cat :any] :boolean])
(m/=> hive-addon.protocol/degraded? [:=> [:cat :any] :boolean])
(m/=> hive-addon.protocol/down? [:=> [:cat :any] :boolean])

(def WireValue
  "What json-safe folds anything into: nil, string, boolean, number, keyword,
   or vectors / string-or-keyword-keyed maps of the same."
  [:schema {:registry {::wire [:or :nil :string :boolean number? :keyword
                               [:vector [:ref ::wire]]
                               [:map-of [:or :string :keyword] [:ref ::wire]]]}}
   ::wire])

(m/=> hive-addon.wire/json-safe [:=> [:cat :any] WireValue])
