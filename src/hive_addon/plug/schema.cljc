(ns hive-addon.plug.schema
  "Malli schemas for iaddon.edn (:iaddon/* keys), in a local composite registry.
   Credential values never appear here — only reference chains."
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr]
            [hive-dsl.result :as r]))

;; SPDX-License-Identifier: MIT

(def LibSym :qualified-symbol)
(def AddonId [:string {:min 1}])
(def TrustClass [:enum :native :foss :proprietary :external])
(def Capability :keyword)

(def LocalRoot [:map {:closed false} [:local/root :string]])
(def GitCoord  [:map {:closed false}
                [:git/url :string]
                [:git/sha {:optional true} [:string {:min 7}]]
                [:git/tag {:optional true} :string]])
(def MvnCoord  [:map {:closed false} [:mvn/version [:string {:min 1}]]])
(def Source    [:or LocalRoot GitCoord MvnCoord])

(def Verify [:map {:closed false}
             [:sha256 {:optional true} :string]
             [:sig    {:optional true} :string]
             [:signer {:optional true} :string]])

(def Plug
  [:map {:closed false}
   [:enabled?     {:optional true} :boolean]
   [:source       Source]
   [:addon/id     {:optional true} AddonId]
   [:class        {:optional true} TrustClass]
   [:capability   {:optional true} Capability]
   [:repo         {:optional true} :keyword]
   [:credential   {:optional true} :keyword]
   [:verify       {:optional true} Verify]
   [:capabilities {:optional true} [:set Capability]]
   [:config       {:optional true} [:map {:closed false}]]])

(def Plugs [:map-of LibSym Plug])

(def RepoDef [:map {:closed false} [:url :string] [:private {:optional true} :boolean]])
(def Repos   [:map-of :keyword RepoDef])

(def CredStep
  "A chain step: a reference source-type keyword then its lookup args, never a value."
  [:catn [:type :keyword] [:args [:* :any]]])
(def CredDef     [:map {:closed false} [:type {:optional true} :keyword] [:chain [:sequential CredStep]]])
(def Credentials [:map-of :keyword CredDef])

(def CapabilityRule [:map {:closed false}
                     [:permit {:optional true} :boolean]
                     [:prefer {:optional true} LibSym]])
(def Capabilities [:map-of Capability CapabilityRule])

(def Trust [:map {:closed false}
            [:keyring           {:optional true} :string]
            [:require-signature {:optional true} [:set TrustClass]]
            [:allow-unsigned    {:optional true} [:set TrustClass]]
            [:pinned            {:optional true} [:map-of AddonId :string]]])

(def Profile [:map {:closed false}
              [:only   {:optional true} [:set :qualified-symbol]]
              [:except {:optional true} [:set :qualified-symbol]]
              [:all    {:optional true} :boolean]])
(def Profiles [:map-of :keyword Profile])

(def IaddonConfig
  [:map {:closed false}
   [:iaddon/plugs        {:optional true} Plugs]
   [:iaddon/repos        {:optional true} Repos]
   [:iaddon/credentials  {:optional true} Credentials]
   [:iaddon/capabilities {:optional true} Capabilities]
   [:iaddon/trust        {:optional true} Trust]
   [:iaddon/profiles     {:optional true} Profiles]
   [:iaddon/lock         {:optional true} :any]
   [:iaddon/module       {:optional true} [:or :keyword :string]]])

;; =============================================================================
;; Lint findings, and the arglists of the portable plug subjects
;; =============================================================================

(def LintRule
  "Rule ids `hive-addon.plug.lint` can report. A CLOSED set on purpose: the rule
   chain is fixed here, and a lint result naming an unregistered rule is a bug
   rather than an extension point."
  [:enum :literal-secret :ambiguous-source :source-family-mismatch :mutable-tag])

(def Violation
  "One lint finding. The subject key varies by rule — :lib, :repo or :credential
   — so exactly one of them is present alongside the rule and its detail."
  [:map {:closed false}
   [:rule LintRule]
   [:detail [:string {:min 1}]]
   [:lib        {:optional true} LibSym]
   [:repo       {:optional true} :any]
   [:credential {:optional true} :any]])

(def Violations [:sequential Violation])

(def LintArgs
  "Arglist of hive-addon.plug.lint/check. A :cat schema, so a schema-driven test
   APPLIES the subject rather than handing it the vector as one argument."
  [:cat IaddonConfig])

(def CoordSourceArgs
  "Arglist of hive-addon.plug.source/coord->source. Deliberately :any rather
   than Source: the function's whole job is to CLASSIFY an arbitrary coord map
   and answer nil for one it does not recognize, so constraining the input to
   already-valid coords would remove the case under test."
  [:cat [:map-of :any :any]])

(def DeepMergeArgs
  "Arglist of hive-addon.plug.merge/deep-merge, stated at its BINARY arity.

   The function is variadic, but a `[:* ...]` tail cannot be driven by a
   schema-derived argument generator (it has no fixed arity to build a tuple
   from). The binary case carries the whole relation — deep-merge is a fold of
   it — and the variadic fold plus nil-dropping are covered by the structured
   properties in hive-addon.plug.portable-trifecta-test.

   `:maybe` is deliberate: nil is meaningful input, dropped rather than merged."
  [:cat [:maybe [:map-of :any :any]] [:maybe [:map-of :any :any]]])

(def ^:private plug-schemas
  {:iaddon/lib-sym LibSym :iaddon/addon-id AddonId :iaddon/trust-class TrustClass
   :iaddon/source Source :iaddon/verify Verify :iaddon/plug Plug :iaddon/plugs Plugs
   :iaddon/repos Repos :iaddon/cred-step CredStep :iaddon/cred-def CredDef
   :iaddon/credentials Credentials :iaddon/capability-rule CapabilityRule
   :iaddon/capabilities Capabilities :iaddon/trust-config Trust
   :iaddon/profile Profile :iaddon/profiles Profiles :iaddon/config IaddonConfig
   :iaddon/lint-rule LintRule :iaddon/violation Violation
   :iaddon/violations Violations :iaddon/lint-args LintArgs
   :iaddon/coord-source-args CoordSourceArgs :iaddon/deep-merge-args DeepMergeArgs})

(def registry
  (mr/composite-registry (m/default-schemas) (mr/registry plug-schemas)))

(defn schema  [?s]   (m/schema ?s {:registry registry}))
(defn validate [?s x] (m/validate ?s x {:registry registry}))
(defn explain  [?s x] (m/explain ?s x {:registry registry}))
(defn humanize-errors [?s x] (some-> (explain ?s x) me/humanize))

(defn validate*
  "Validate x against ?s, bridging to hive-dsl Result (category defaults to
   :iaddon/schema-violation)."
  ([?s x] (validate* ?s x :iaddon/schema-violation))
  ([?s x category]
   (if (validate ?s x) (r/ok x) (r/err category {:explanation (humanize-errors ?s x)}))))
