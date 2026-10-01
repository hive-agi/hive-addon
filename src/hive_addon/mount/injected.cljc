(ns hive-addon.mount.injected
  "The injected-spec registry: MountSpecs that reached the running image by
   hot injection rather than by being on the classpath at boot.

   Why it exists. An injected addon's manifest lives under a URL added to ONE
   DynamicClassLoader — the injecting thread's. Classpath discovery run from any
   other thread (an MCP request thread, a reload triggered by a file watcher)
   never sees it, so the addon silently drops out of every later reload, doctor
   report and effective-spec computation. Remembering the spec as DATA, keyed by
   addon id, and unioning it into discovery makes an injected addon a first-class
   citizen for the rest of the image's life — until it is ejected.

   Strata:
     Value object  InjectedEntry  {:addon/id .. :spec MountSpec :hot/path ..
                                   :hot/paths [..] :hot/classpath [url ..]
                                   :hot/dirs [dir ..]}
     Pure          union-specs, entries-for-path
     Boundary      remember! / forget! / entries / injected-specs over one atom

   Portable stratum: no reader conditionals, no host API.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Value object
;; =============================================================================

(def InjectedEntry
  "One injected addon, as remembered. Open."
  [:map {:closed false}
   [:addon/id [:string {:min 1}]]
   [:spec [:map {:closed false} [:addon/id [:string {:min 1}]]]]
   [:hot/path {:optional true} :string]
   [:hot/paths {:optional true} [:sequential :string]]
   [:hot/classpath {:optional true} [:sequential :string]]
   [:hot/dirs {:optional true} [:sequential :string]]])

;; =============================================================================
;; Pure
;; =============================================================================

(defn union-specs
  "CLASSPATH specs plus every INJECTED spec whose id the classpath does not
   already answer. The classpath wins a tie: a spec that is on the classpath is
   the authoritative manifest, the injected one only a memory of it."
  [classpath injected]
  (let [seen (into #{} (map :addon/id) classpath)]
    (into (vec classpath)
          (remove #(contains? seen (:addon/id %)))
          injected)))

(defn entries-for-path
  "Entries injected from PATH (compared as given — callers canonicalize)."
  [entries path]
  (filterv #(= path (:hot/path %)) entries))

;; =============================================================================
;; Boundary — one atom, keyed by addon id
;; =============================================================================

(defonce ^:private registry (atom {}))

(defn remember!
  "Record ENTRIES (InjectedEntry maps). Re-injecting an id replaces its entry.
   Returns the remembered ids."
  [entries]
  (swap! registry into (map (juxt :addon/id identity)) entries)
  (mapv :addon/id entries))

(defn forget!
  "Drop IDS. Returns the ids that were actually remembered."
  [ids]
  (let [[old _] (swap-vals! registry #(apply dissoc % ids))]
    (into [] (filter #(contains? old %)) ids)))

(defn entries
  "Every remembered InjectedEntry, ordered by id."
  []
  (mapv val (sort-by key @registry)))

(defn entry
  "The InjectedEntry for ID, or nil."
  [id]
  (get @registry id))

(defn injected-specs
  "The remembered MountSpecs, ordered by id."
  []
  (mapv :spec (entries)))

(defn reset-registry!
  "Forget everything. For tests."
  []
  (reset! registry {})
  nil)
