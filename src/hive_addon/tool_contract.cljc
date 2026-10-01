(ns hive-addon.tool-contract
  "The root-tool contract: what a tool def must carry to be served at an MCP
   root `tools/list`.

   A root tool needs a non-blank :name and :description and an :inputSchema of
   type \"object\" with at least one property. Anything less is a VIOLATION.
   A property without a :description is a WARNING.

   Enforcement points, all reading `RootToolDef`:
     host registration   `assert-root-tool!` / `assert-root-tools!`
     load / compile time  `def-root-tool`
     an addon's CI        `hive-addon.tool-contract.test/deftest-root-tools`,
                          `hive-addon.tool-contract.check/-main`"
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Value objects
;; =============================================================================

(def NonBlankString
  "A string with at least one non-whitespace character."
  [:and
   [:string {:error/message "must be a string"}]
   [:fn {:error/message "must not be blank"} (complement str/blank?)]])

(def PropertyName
  "A JSON-schema property key, as a string or keyword."
  [:or :string :keyword])

(def Property
  "One property's JSON schema. Open: any JSON-schema keys are allowed."
  [:map {:closed false}])

(def Properties
  "The :properties of a root tool's :inputSchema. Never empty."
  [:and
   [:map-of {:error/message "must be a map of property name to schema map"}
    PropertyName Property]
   [:fn {:error/message "must declare at least one property"} seq]])

(def RootInputSchema
  "The :inputSchema a root tool must carry."
  [:map {:closed false}
   [:type [:enum {:error/message "must be \"object\""} "object" :object]]
   [:properties Properties]])

(def RootToolDef
  "A tool def fit for an MCP root. Open: hosts and addons add keys."
  [:map {:closed false}
   [:name NonBlankString]
   [:description NonBlankString]
   [:inputSchema RootInputSchema]])

(def Finding
  "One contract finding: where it is and what is wrong."
  [:map
   [:path [:vector :any]]
   [:message :string]])

;; =============================================================================
;; Promote (pure)
;; =============================================================================

(def ^:private root-tool-validator (m/validator RootToolDef))
(def ^:private root-tool-explainer (m/explainer RootToolDef))

(defn- error->finding [error]
  {:path    (vec (:in error))
   :message (or (me/error-message error)
                (if (= ::m/missing-key (:type error))
                  "is required"
                  "is invalid"))})

(defn violations
  "Contract violations of `tool` as a vector of Findings; [] when it conforms."
  [tool]
  (if (root-tool-validator tool)
    []
    (->> (:errors (root-tool-explainer tool))
         (mapv error->finding)
         distinct
         vec)))

(defn warnings
  "Advisory findings of `tool`: each property lacking a non-blank :description."
  [tool]
  (let [props (get-in tool [:inputSchema :properties])]
    (if (map? props)
      (->> props
           (keep (fn [[k v]]
                   (let [d (when (map? v) (:description v))]
                     (when-not (and (string? d) (not (str/blank? d)))
                       {:path    [:inputSchema :properties k :description]
                        :message "property has no description"}))))
           (sort-by (comp str peek pop :path))
           vec)
      [])))

(defn conforms?
  "True when `tool` has no contract violations."
  [tool]
  (boolean (root-tool-validator tool)))

(defn tool-label
  "The tool's :name for messages, or a placeholder when it has none."
  [tool]
  (let [n (when (map? tool) (:name tool))]
    (if (and (string? n) (not (str/blank? n))) n "<unnamed tool>")))

(defn report
  "Check every tool in `tools`.
   => {:checked n :violations {label [Finding]} :warnings {label [Finding]}}"
  [tools]
  (let [tools (vec tools)
        by    (fn [f] (into (sorted-map)
                            (keep (fn [t] (let [fs (f t)]
                                            (when (seq fs) [(tool-label t) fs]))))
                            tools))]
    {:checked    (count tools)
     :violations (by violations)
     :warnings   (by warnings)}))

(defn format-findings
  "Render {label [Finding]} as one line per finding."
  [label->findings]
  (str/join "\n"
            (for [[label fs] label->findings
                  {:keys [path message]} fs]
              (str "  " label " " (pr-str path) ": " message))))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn- contract-error [label->findings]
  (ex-info (str "MCP root tool contract violated — a client without source "
                "access operates a tool only through its schema.\n"
                (format-findings label->findings))
           {:type       :hive-addon/root-tool-contract
            :violations label->findings}))

(defn assert-root-tool!
  "Return `tool` when it conforms; throw ex-info
   {:type :hive-addon/root-tool-contract :violations {label [Finding]}} otherwise."
  [tool]
  (let [vs (violations tool)]
    (if (seq vs)
      (throw (contract-error {(tool-label tool) vs}))
      tool)))

(defn assert-root-tools!
  "Return `tools` when every tool conforms; otherwise throw ONE ex-info naming
   every offending tool (same ex-data shape as `assert-root-tool!`)."
  [tools]
  (let [{vs :violations} (report tools)]
    (if (seq vs)
      (throw (contract-error vs))
      tools)))

;; =============================================================================
;; Compile time
;; =============================================================================

(def ^:private conforming-stand-ins
  "Values that satisfy each contract key; replace non-literal forms so a
   macroexpansion-time check can only fail on what the literal source says."
  {:name        "stand-in"
   :description "stand-in"
   :inputSchema {:type "object" :properties {"stand-in" {}}}})

(defn literal-form?
  "True when `form` contains no symbol and no list: its value is the form."
  [form]
  (not-any? #(or (symbol? %) (seq? %))
            (tree-seq coll? seq form)))

(defn expansion-view
  "The tool def as far as source text decides it: a map literal with each
   non-literal contract value replaced by a conforming stand-in. nil when
   `form` is not a map literal."
  [form]
  (when (map? form)
    (reduce-kv (fn [m k stand-in]
                 (if (and (contains? m k) (not (literal-form? (get m k))))
                   (assoc m k stand-in)
                   m))
               form
               conforming-stand-ins)))

#?(:clj
   (defmacro def-root-tool
     "Def `sym` to `tool-def`, checked against the root-tool contract.
      A map literal is checked at macroexpansion, so a violation the source
      text already decides fails compilation; the evaluated value is checked
      again when the def runs."
     [sym tool-def]
     (when-let [view (expansion-view tool-def)]
       (when-let [vs (seq (violations view))]
         (throw (contract-error {(tool-label view) (vec vs)}))))
     `(def ~sym (assert-root-tool! ~tool-def))))
