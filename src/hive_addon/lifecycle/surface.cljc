(ns hive-addon.lifecycle.surface
  "An addon's Surface: the tools and contributed commands a caller can reach it
   through. Pure. A dormant addon is advertised from its surface, so a surface
   must be known without mounting.

   Portable stratum: no reader conditionals, no `for`."
  (:require [hive-addon.wire :as wire]
            [hive-addon.lifecycle.schema :as ls]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def empty-surface {:tools [] :commands {}})

(defn tool-decl
  "TOOL-DEF as advertisable data: no handler, nothing that cannot cross a wire."
  [tool-def]
  (wire/json-safe (dissoc tool-def :handler)))

(defn normalize
  "SURFACE with both keys present, tool-defs stripped, command names distinct and
   sorted, empty command lists dropped."
  [surface]
  {:tools    (mapv tool-decl (:tools surface []))
   :commands (into {}
                   (comp (map (fn [[tool cmds]] [(str tool) (vec (sort (distinct (map str cmds))))]))
                         (filter (fn [[_ cmds]] (seq cmds))))
                   (:commands surface {}))})

(defn blank?
  "True when SURFACE advertises nothing."
  [surface]
  (or (nil? surface)
      (and (empty? (:tools surface))
           (every? empty? (vals (:commands surface {}))))))

(defn well-formed?
  "True when S has the Surface shape normalize can read."
  [s]
  (and (map? s)
       (let [tools (:tools s [])
             cmds  (:commands s {})]
         (and (sequential? tools)
              (every? #(and (map? %) (string? (:name %)) (seq (:name %))) tools)
              (map? cmds)
              (every? (fn [[k v]] (and (or (string? k) (keyword? k))
                                       (sequential? v)
                                       (every? #(or (string? %) (keyword? %)) v)))
                      cmds)))))

(defn declared
  "The manifest-declared surface of SPEC, normalized, or nil when absent,
   malformed, or empty."
  [spec]
  (let [s (:addon/surface spec)]
    (when (well-formed? s)
      (let [n (normalize s)]
        (when-not (blank? n) n)))))

(defn observed
  "The surface ADDON-ID shows while mounted: TOOLS it returns, plus every command
   in CONTRIBUTIONS ({tool-name {command-name {:addon id ...}}}) tagged with it."
  [addon-id tools contributions]
  (normalize
   {:tools    tools
    :commands (into {}
                    (keep (fn [[tool cmds]]
                            (let [mine (into []
                                             (comp (filter (fn [[_ spec]] (= addon-id (:addon spec))))
                                                   (map key))
                                             cmds)]
                              (when (seq mine) [tool mine]))))
                    contributions)}))

(defn resolve-surface
  "[surface source] for SPEC given the LEARNED surface (or nil). Declared wins."
  [spec learned]
  (if-let [d (declared spec)]
    [d :declared]
    (if (or (not (well-formed? learned)) (blank? learned))
      [nil :none]
      [(normalize learned) :learned])))

;; =============================================================================
;; Contracts
;; =============================================================================
;; Schemas come from hive-addon.lifecycle.schema, the lifecycle's own value
;; objects, so the contract and the registry cannot drift apart.

(def ^:private NormalSurface
  "What `normalize` answers: both keys present, string command keys and names."
  [:map {:closed true}
   [:tools [:vector ls/ToolDecl]]
   [:commands [:map-of :string [:vector {:min 1} :string]]]])

;; tool-decl, normalize, blank?, well-formed? and declared are TOTAL over
;; their input: they are what the lifecycle runs on a manifest nobody
;; validated, and each answers nil/false rather than throwing.
(m/=> tool-decl [:=> [:cat [:map {:closed false}]] [:map {:closed false}]])
(m/=> normalize [:=> [:cat [:maybe [:map {:closed false}
                                     [:tools {:optional true} [:maybe [:sequential ls/ToolDecl]]]
                                     [:commands {:optional true}
                                      [:maybe [:map-of :any [:sequential :any]]]]]]]
                 NormalSurface])
(m/=> blank? [:=> [:cat [:maybe [:map {:closed false}]]] :boolean])
(m/=> well-formed? [:=> [:cat :any] :boolean])
(m/=> declared [:=> [:cat [:maybe [:map {:closed false}]]] [:maybe NormalSurface]])
(m/=> observed [:=> [:cat :any [:sequential ls/ToolDecl]
                     [:maybe [:map-of :any [:map-of :any [:maybe [:map {:closed false}]]]]]]
                NormalSurface])
(m/=> resolve-surface [:=> [:cat [:maybe [:map {:closed false}]] :any]
                       [:or [:tuple NormalSurface [:enum :declared :learned]]
                        [:tuple :nil [:= :none]]]])
