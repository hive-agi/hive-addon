(ns hive-addon.lifecycle.surface
  "An addon's Surface: the tools and contributed commands a caller can reach it
   through. Pure. A dormant addon is advertised from its surface, so a surface
   must be known without mounting.

   Portable stratum: no reader conditionals, no `for`."
  (:require [hive-addon.wire :as wire]))

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
