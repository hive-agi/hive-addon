(ns hive-addon.extension
  "Extension-point vocabulary: what an addon ACCEPTS providers for.

   A manifest carries three capability edges. `:addon/capabilities` is what an
   addon OFFERS, `:addon/requires-capabilities` what it needs a HOST to offer,
   and `:addon/extension-points` what it accepts PROVIDERS for. This namespace
   owns the third and the derivations over it.

   Contract: PURE. Value objects are uncompiled malli DATA (house idiom:
   PascalCase defs) seeded into a LOCAL composite registry that COMPOSES
   hive-addon.schema's registry under :extension/* keys. The registry is NEVER
   installed as the malli global default. Derivations take manifest maps and
   return data, with no registry mutation, no IO and no live fns."
  (:require [clojure.set :as set]
            [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr]
            [hive-dsl.adt :as adt]
            [hive-dsl.result :as r]
            [hive-addon.protocol :as proto]
            [hive-addon.schema :as s]
            [hive-addon.capability :as cap]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Closed sums
;; =============================================================================

(adt/defadt Cardinality
  "How many providers may fill one extension point at once."
  :cardinality/one :cardinality/many)

(def cardinalities
  "Set of every :cardinality/* variant keyword."
  (adt/type-variants :Cardinality))

(def CardinalityKw
  "Enum schema over `cardinalities`."
  (into [:enum] (sort cardinalities)))

;; =============================================================================
;; Value objects: uncompiled malli DATA (house idiom: PascalCase defs)
;; =============================================================================

(def ExtensionPoint
  "ONE seam an addon opens for providers.

   `:extension/capability` is the join key: a provider fills this point by
   declaring the same keyword in its own `:addon/capabilities`.

   `:extension/port` names the protocol a provider implements and
   `:extension/registry` the namespace it registers itself in. Both are STRINGS,
   because a manifest is data and dereferences no symbol.

   `:extension/schema` is a registry key for the parameters a provider
   contributes, never the schema value itself.

   `:extension/docs` is a classpath-relative resource inside the addon's own
   artifact, so the prose travels with the version that declares it."
  [:map {:closed false}
   [:extension/capability  s/Capability]
   [:extension/summary     [:string {:min 8 :max 200}]]
   [:extension/port        {:optional true} [:string {:min 1}]]
   [:extension/registry    {:optional true} [:string {:min 1}]]
   [:extension/schema      {:optional true} :qualified-keyword]
   [:extension/cardinality {:optional true :default :cardinality/many} CardinalityKw]
   [:extension/stability   {:optional true :default :stability/stable} cap/StabilityKw]
   [:extension/since       {:optional true} [:string {:min 1}]]
   [:extension/docs        {:optional true} [:string {:min 1}]]])

(defn- distinct-capabilities?
  "True when no `:extension/capability` appears twice in `points`."
  [points]
  (let [cs (map :extension/capability points)]
    (= (count cs) (count (distinct cs)))))

(def ExtensionPoints
  "The seams one addon opens, at most one per capability."
  [:and
   [:vector ExtensionPoint]
   [:fn {:error/message "duplicate :extension/capability in :addon/extension-points"}
    distinct-capabilities?]])

;; =============================================================================
;; Collect: read what a manifest declares
;; =============================================================================

(defn points-of
  "`spec` -> its declared ExtensionPoints, or []."
  [spec]
  (:addon/extension-points spec []))

(defn point-capabilities
  "`spec` -> the set of capabilities it accepts providers for."
  [spec]
  (into #{} (map :extension/capability) (points-of spec)))

(defn index
  "`spec` -> {capability ExtensionPoint}."
  [spec]
  (into {} (map (juxt :extension/capability identity)) (points-of spec)))

;; =============================================================================
;; Promote: pure derivations over a spec set
;; =============================================================================

(defn openers
  "`specs` -> {capability #{addon-id}}: who ACCEPTS providers for each."
  [specs]
  (reduce (fn [acc spec]
            (reduce (fn [a c] (update a c (fnil conj #{}) (:addon/id spec)))
                    acc (point-capabilities spec)))
          {} specs))

(defn providers
  "`specs` -> {capability #{addon-id}}: who OFFERS each capability."
  [specs]
  (reduce (fn [acc spec]
            (reduce (fn [a c] (update a c (fnil conj #{}) (:addon/id spec)))
                    acc (:addon/capabilities spec #{})))
          {} specs))

(defn tenants
  "The addon ids in `specs` that fill `capability`, excluding the opener itself."
  [specs capability]
  (let [filling (get (providers specs) capability #{})
        opening (get (openers specs) capability #{})]
    (set/difference filling opening)))

(defn- merge-by-id
  "[[addon-id v] ...] -> {addon-id v}, combining the values of a repeated id
   with `f` rather than keeping only the last."
  [f entries]
  (reduce (fn [acc [id v]]
            (if (contains? acc id) (update acc id f v) (assoc acc id v)))
          {} entries))

(defn vacant
  "`specs` -> {addon-id #{capability}}: points opened with no tenant.

   The mirror of a mount plan's `:unmet-capabilities`. A vacant point is a seam
   no provider has ever exercised end to end. Specs sharing an :addon/id
   contribute to ONE entry, the union of their empty points."
  [specs]
  (merge-by-id set/union
               (keep (fn [spec]
                       (let [empty-points (into #{}
                                                (remove #(seq (tenants specs %)))
                                                (point-capabilities spec))]
                         (when (seq empty-points) [(:addon/id spec) empty-points])))
                     specs)))

(defn- accounted
  "The capabilities `specs` reference at all: opened as a point, required of a
   host, or standard to the protocol."
  [specs]
  (into (set proto/standard-capabilities)
        cat
        [(keys (openers specs))
         (mapcat :addon/requires-capabilities specs)]))

(defn unconsumed
  "`specs` -> {addon-id #{capability}}: offered, and referenced by nothing.

   The mirror of `vacant`. A capability here is neither some addon's extension
   point, nor any addon's `:addon/requires-capabilities`, nor standard, so
   nothing in the system can reach what this addon registered. Specs sharing
   an :addon/id contribute to ONE entry, the union of their stray offers."
  [specs]
  (let [known (accounted specs)]
    (merge-by-id set/union
                 (keep (fn [spec]
                         (let [stray (into #{} (remove known) (:addon/capabilities spec #{}))]
                           (when (seq stray) [(:addon/id spec) stray])))
                       specs))))

(defn over-subscribed
  "`specs` -> {addon-id {capability #{tenant-id}}}: `:cardinality/one` points
   filled by more than one provider. Specs sharing an :addon/id contribute to
   ONE entry."
  [specs]
  (merge-by-id (partial merge-with set/union)
               (keep (fn [spec]
                       (let [clashes (into {}
                                           (keep (fn [[c point]]
                                                   (when (= :cardinality/one
                                                            (:extension/cardinality point :cardinality/many))
                                                     (let [ts (tenants specs c)]
                                                       (when (> (count ts) 1) [c ts])))))
                                           (index spec))]
                         (when (seq clashes) [(:addon/id spec) clashes])))
                     specs)))

(defn report
  "`specs` -> {:openers :providers :vacant :unconsumed :over-subscribed}.

   Everything derived, nothing stored."
  [specs]
  {:openers         (openers specs)
   :providers       (providers specs)
   :vacant          (vacant specs)
   :unconsumed      (unconsumed specs)
   :over-subscribed (over-subscribed specs)})

;; =============================================================================
;; Local composite registry: hive-addon.schema registry + :extension/* schemas
;; =============================================================================

(def ^:private extension-schemas
  "Static :extension/* -> schema map seeded into the local registry."
  {:extension/cardinality CardinalityKw
   :extension/point       ExtensionPoint
   :extension/points      ExtensionPoints})

(def registry
  "Composite malli registry: hive-addon.schema's registry plus this ns's
   :extension/* schemas. NOT installed as the global default."
  (mr/composite-registry
   s/registry
   (mr/registry extension-schemas)))

(defn schema
  "Compile ?s against the local :extension/* + :addon/* registry."
  [?s]
  (m/schema ?s {:registry registry}))

(defn validate
  "Registry-aware validate. True or false."
  [?s x]
  (m/validate ?s x {:registry registry}))

(defn explain
  "Registry-aware explain. Nil on success, error map on failure."
  [?s x]
  (m/explain ?s x {:registry registry}))

(defn humanize-errors
  "Human-readable error data for x against ?s, or nil if x conforms."
  [?s x]
  (some-> (explain ?s x) me/humanize))

(defn validate*
  "Validate x against ?s, bridging to hive-dsl Result.
   (r/ok x) on success; (r/err category {:explanation <humanized>}) on failure.
   `category` defaults to :extension/schema-violation."
  ([?s x] (validate* ?s x :extension/schema-violation))
  ([?s x category]
   (if (validate ?s x)
     (r/ok x)
     (r/err category {:explanation (humanize-errors ?s x)}))))

(def valid-point?
  "x -> boolean. Compiled ExtensionPoint validator."
  (m/validator (schema ExtensionPoint) {:registry registry}))

(m/=> points-of          [:=> [:cat [:map-of :any :any]] [:sequential :any]])
(m/=> point-capabilities [:=> [:cat [:map-of :any :any]] [:set :keyword]])
(m/=> tenants            [:=> [:cat [:sequential :any] :keyword] [:set :string]])
