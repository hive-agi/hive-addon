(ns hive-addon.hot.cascade
  "Which addons a reload actually touches — the pure Pipeline stratum of the
   hot-reload bridge. No IO, no var resolution, no host.

   Reloading addon A is not enough. Every addon that received A's INSTANCE at
   mount time (through the mounter's `:mount/dependencies` sibling injection)
   still holds the pre-reload object; leaving them alone leaves the system half
   old and half new, which is the same silent-corruption shape as a partial
   namespace reload. So a reload seeds at the changed addons and closes forward
   over the dependency graph.

   The graph is NOT recomputed here: `edges` and `solve` come from
   hive-addon.mount.solve, so the order a reload uses is the same order the
   original mount used, produced by the same rule chain (and extended by the
   same custom `:rules`)."
  (:require [hive-addon.mount.solve :as solve]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Forward closure over the dependency edge set
;; =============================================================================

(defn- adjacency
  "Edge set #{[from to]} -> {from #{to ...}}, where an edge means `from` mounts
   before `to`, i.e. `to` DEPENDS ON `from`."
  [edge-set]
  (reduce (fn [m [from to]] (update m from (fnil conj #{}) to)) {} edge-set))

(defn dependents
  "The transitive dependent closure of `seed-ids` within `specs`, INCLUDING the
   seeds themselves.

   Follows dependency edges forward: if B depends on A and C depends on B, then
   (dependents specs #{\"A\"}) is #{\"A\" \"B\" \"C\"}. Terminates on cycles because
   the frontier only ever advances into ids not already seen.

   opts: {:rules [rule ...]} — the same OCP rule chain `solve` takes, so a custom
   ordering rule contributes to the reload closure too."
  ([specs seed-ids] (dependents specs seed-ids {}))
  ([specs seed-ids {:keys [rules]}]
   (let [edge-set (if rules (solve/edges specs rules) (solve/edges specs))
         adj      (adjacency edge-set)
         seeds    (set seed-ids)]
     (loop [frontier seeds
            seen     seeds]
       (let [next-ids (into #{} (comp (mapcat adj) (remove seen)) frontier)]
         (if (empty? next-ids)
           seen
           (recur next-ids (into seen next-ids))))))))

;; =============================================================================
;; The plan a reload drives
;; =============================================================================

(defn affected-plan
  "A MountPlan whose :ordered is the reload closure of `seed-ids`, in the SAME
   global topological order the full mount used.

   Built by solving the WHOLE spec set and then filtering :ordered down to the
   closure — never by solving the subset alone. Solving the subset would drop the
   ordering constraints contributed by specs outside it, so a dependent could be
   remounted before a dependency that merely happened not to be affected.

   Diagnostics (:cycles, :missing, :unmet-capabilities, :duplicates) are carried
   through from the full solve, unfiltered: they describe the graph, not the
   slice.

   opts is passed to `solve` (:rules, :fail-closed-cycles)."
  ([specs seed-ids] (affected-plan specs seed-ids {}))
  ([specs seed-ids opts]
   (let [affected (dependents specs seed-ids opts)
         plan     (solve/solve specs opts)]
     (assoc plan
            :ordered (filterv #(contains? affected (:addon/id %)) (:ordered plan))
            :hot/affected-ids affected))))

;; =============================================================================
;; ns -> addon lookup (what a namespace reload seeds)
;; =============================================================================

(defn ns->addon-ids
  "Index {init-ns-string #{addon-id ...}} over `specs`.

   A set, not a single id, because two addons legitimately share a constructor
   namespace — `hive.qdrant` and `hive.qdrant.kanban` both construct from
   `hive-qdrant.addon`. Reloading that namespace must seed BOTH."
  [specs]
  (reduce (fn [m spec]
            (if-let [ns-str (:addon/init-ns spec)]
              (update m ns-str (fnil conj #{}) (:addon/id spec))
              m))
          {}
          specs))

(defn seeds-for-ns
  "The addon ids whose constructor namespace is `ns-str`. Empty set when none."
  [specs ns-str]
  (get (ns->addon-ids specs) (str ns-str) #{}))
