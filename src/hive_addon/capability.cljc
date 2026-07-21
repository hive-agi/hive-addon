(ns hive-addon.capability
  "Capability contract vocabulary: the declared, machine-readable description of
   what one MCP tool's commands are, what they accept, and how stable they are.

   Contract: a manifest is DATA. It holds no handlers and never dereferences a
   :schema — schema refs are registry keys, resolved by hive-spi.schema.capability.
   Deps: hive-dsl + malli only."
  (:require [clojure.set :as set]
            [malli.core :as m]
            [hive-dsl.adt :as adt]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Closed sums
;; =============================================================================

(adt/defadt Stability
  "Lifecycle position of a command."
  :stability/experimental :stability/stable :stability/deprecated :stability/internal)

(adt/defadt Effect
  "What invoking a command can touch."
  :effect/read :effect/write :effect/index :effect/exec :effect/network)

(adt/defadt DeclarationStatus
  "Declaration state of a command. Derived, never declared."
  :capability/declared :capability/undeclared)

(def stabilities
  "Set of every :stability/* variant keyword."
  (adt/type-variants :Stability))

(def effects
  "Set of every :effect/* variant keyword."
  (adt/type-variants :Effect))

(def StabilityKw
  "Enum schema over `stabilities`."
  (into [:enum] (sort stabilities)))

(def EffectKw
  "Enum schema over `effects`."
  (into [:enum] (sort effects)))

;; =============================================================================
;; Value objects — uncompiled malli DATA (house idiom: PascalCase defs)
;; =============================================================================

(def default-envelope
  "Cross-cutting keys present on EVERY call that no op-schema declares:
   :command/:timeout/:timeout-ms from the dispatcher, :agent_id/:_caller_cwd
   injected by bb-mcp. Subtracted before any unknown-key report."
  #{:command :timeout :timeout-ms :agent_id :_caller_cwd})

(def CommandName
  "A dispatch token exactly as the caller spells it. NOT kebab-constrained:
   \"write-form\", \"carto_definition\" and space-joined \"action redo to\" are all
   valid. 1-64 chars, no leading, trailing or doubled whitespace. Shape
   conventions are a lint rule, not a validity gate."
  [:and [:string {:min 1 :max 64}]
   [:re {:error/message "no leading/trailing/double whitespace"} #"^\S+( \S+)*$"]])

(def AliasSpec
  "Value of the :aliases ENTRY-property on an op-schema param. Either a bare
   alias keyword or a map carrying deprecation metadata. Aliases fold to
   canonical BEFORE validation and render in help; they are never emitted as
   separate JSON-Schema properties."
  [:or :keyword
   [:map {:closed true}
    [:alias :keyword]
    [:kind  {:optional true} [:enum :snake-case :deprecated :synonym]]
    [:since {:optional true} :string]
    [:replaced-in {:optional true} :string]]])

(def CommandExample
  "A runnable invocation. :args is lint-validated against the command's
   resolved :schema."
  [:map {:closed false}
   [:args [:map-of :keyword :any]]
   [:note {:optional true} [:string {:min 1}]]])

(def CommandSpec
  "ONE dispatchable subcommand.

   :schema is OPTIONAL — a command is declarable by summary alone, and the
   absence of :schema IS :capability/undeclared.

   :handler is deliberately ABSENT: a manifest carries no live fns.

   :summary is bounded at 8-160 chars."
  [:map {:closed false}
   [:command     CommandName]
   [:summary     [:string {:min 8 :max 160}]]
   [:schema      {:optional true} :qualified-keyword]
   [:examples    {:optional true} [:vector CommandExample]]
   [:stability   {:optional true :default :stability/stable} StabilityKw]
   [:effects     {:optional true :default #{:effect/read}} [:set EffectKw]]
   [:group       {:optional true} [:string {:min 1}]]
   [:since       {:optional true} :string]
   [:replaced-by {:optional true} CommandName]])

(defn- distinct-commands?
  "True when no :command appears twice in :commands."
  [{:keys [commands]}]
  (let [cs (map :command commands)] (= (count cs) (count (distinct cs)))))

(defn- declared-subset?
  "True when every declared :command is a member of :all-commands."
  [{:keys [commands all-commands]}]
  (every? #(contains? all-commands (:command %)) commands))

(def CapabilityManifest
  "One tool's capability surface, contributed by one owner.

   :all-commands is the addon's LIVE dispatch key set — handler keys plus any
   cond-intercepted command such as \"help\".

   Two invariants beyond the map shape: no duplicate :command, and every
   declared :command is a member of :all-commands."
  [:and
   [:map {:closed false}
    [:tool         [:string {:min 1}]]
    [:owner        :qualified-keyword]
    [:description  [:string {:min 1}]]
    [:all-commands [:set CommandName]]
    [:commands     [:vector CommandSpec]]
    [:envelope     {:optional true :default default-envelope} [:set :keyword]]
    [:version      {:optional true} :string]]
   [:fn {:error/message "duplicate :command in manifest"} distinct-commands?]
   [:fn {:error/message "declared :command absent from :all-commands"} declared-subset?]])

;; =============================================================================
;; Derived accessors
;; =============================================================================

(defn status
  "CommandSpec -> :capability/declared when it carries a :schema, else
   :capability/undeclared."
  [{:keys [schema]}]
  (if schema :capability/declared :capability/undeclared))

(defn index
  "manifest -> {command-name CommandSpec}."
  [manifest]
  (into {} (map (juxt :command identity)) (:commands manifest)))

(defn declared
  "manifest -> set of command names carrying a CommandSpec."
  [manifest]
  (into #{} (map :command) (:commands manifest)))

(defn schematized
  "manifest -> set of command names whose CommandSpec carries a :schema."
  [manifest]
  (into #{} (comp (filter :schema) (map :command)) (:commands manifest)))

(defn undeclared
  "manifest -> set of dispatchable command names with no CommandSpec."
  [manifest]
  (set/difference (:all-commands manifest) (declared manifest)))

(defn command-enum
  "manifest -> the CLOSED command enum schema [:enum & sorted :all-commands].
   The single source of a tool's command list."
  [manifest]
  (into [:enum] (sort (:all-commands manifest))))

(defn coverage
  "manifest -> {:tool :total :declared :schematized :undeclared
                :declared-pct :schematized-pct}. Percentages are 1.0 when the
   tool dispatches nothing."
  [manifest]
  (let [t (count (:all-commands manifest))
        d (count (declared manifest))
        s (count (schematized manifest))]
    {:tool (:tool manifest) :total t :declared d :schematized s
     :undeclared (vec (sort (undeclared manifest)))
     :declared-pct    (if (zero? t) 1.0 (double (/ d t)))
     :schematized-pct (if (zero? t) 1.0 (double (/ s t)))}))

;; =============================================================================
;; Validation + mirror bundle
;; =============================================================================

(def valid-manifest?
  "x -> boolean. Compiled CapabilityManifest validator."
  (m/validator CapabilityManifest))

(def explain-manifest
  "x -> malli explanation map, or nil when x conforms to CapabilityManifest."
  (m/explainer CapabilityManifest))

(def manifest-schemas
  "Mirror bundle {registry-key schema} for a host to register under
   :hive.capability/*. hive-addon never registers into hive-spi itself."
  {:hive.capability/manifest CapabilityManifest
   :hive.capability/spec     CommandSpec
   :hive.capability/example  CommandExample})

(m/=> command-enum [:=> [:cat CapabilityManifest] [:vector :any]])
(m/=> coverage     [:=> [:cat CapabilityManifest] [:map [:declared-pct :double]]])
