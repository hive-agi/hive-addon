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

(def ^:private plug-schemas
  {:iaddon/lib-sym LibSym :iaddon/addon-id AddonId :iaddon/trust-class TrustClass
   :iaddon/source Source :iaddon/verify Verify :iaddon/plug Plug :iaddon/plugs Plugs
   :iaddon/repos Repos :iaddon/cred-step CredStep :iaddon/cred-def CredDef
   :iaddon/credentials Credentials :iaddon/capability-rule CapabilityRule
   :iaddon/capabilities Capabilities :iaddon/trust-config Trust
   :iaddon/profile Profile :iaddon/profiles Profiles :iaddon/config IaddonConfig})

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
