(ns hive-addon.mount.compose
  "Composition root: select discovered MountSpecs through declarative plug layers
   (build.edn / iaddon.edn), then order + mount them.

   Joins the two pure engines by :addon/id — hive-addon.plug (SELECT, keyed by
   lib-sym) and hive-addon.mount (ORDER + EXECUTE, keyed by :addon/id). Per-addon
   plug :config overrides merge on top of the injected base config resolver.
   With no plug layers (or none carrying :iaddon/plugs) every discovered spec is
   kept — behaviourally identical to mount/mount-classpath!."
  (:require [hive-addon.plug :as plug]
            [hive-addon.plug.schema :as plug-schema]
            [hive-addon.mount.solve :as solve]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.port :as port]
            [hive-dsl.result :as r]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Pure core — plug ⇄ mount join by :addon/id
;; =============================================================================

(defn plug-index
  "Index a plug resolution (the unwrapped :ok of plug/resolve-config, i.e.
   {:config :selected :dropped :provenance}) by :addon/id. Returns
   {:by-id {addon-id plug} :selected-ids #{addon-id} :dropped-ids #{addon-id}
    :dropped-reasons {addon-id reason} :config-by-id {addon-id config}
    :unjoinable [lib-sym]}. A plug lacking :addon/id cannot bind to a MountSpec →
   its lib-sym is collected in :unjoinable, never thrown."
  [{:keys [config selected dropped]}]
  (let [plug-of (fn [lib] (or (get selected lib)
                              (get-in config [:iaddon/plugs lib])))
        add-selected (fn [acc [lib plug]]
                       (if-let [id (:addon/id plug)]
                         (cond-> (-> acc
                                     (assoc-in [:by-id id] plug)
                                     (update :selected-ids conj id))
                           (:config plug) (assoc-in [:config-by-id id] (:config plug)))
                         (update acc :unjoinable conj lib)))
        add-dropped (fn [acc [lib reason]]
                      (let [plug (plug-of lib)]
                        (if-let [id (:addon/id plug)]
                          (-> acc
                              (assoc-in [:by-id id] plug)
                              (update :dropped-ids conj id)
                              (assoc-in [:dropped-reasons id] reason))
                          (update acc :unjoinable conj lib))))
        init {:by-id {} :selected-ids #{} :dropped-ids #{}
              :dropped-reasons {} :config-by-id {} :unjoinable []}]
    (as-> init acc
      (reduce add-selected acc selected)
      (reduce add-dropped acc dropped))))

(defn select-specs
  "Filter discovered MountSpecs through a plug index.
   Default (strict-select false): plug only REMOVES — keep every spec whose
   :addon/id is NOT in :dropped-ids UNLESS it is also in :selected-ids (a
   selected sibling lib wins over a dropped one sharing the addon-id); a spec
   with no plug entry is kept.
   Strict (strict-select true): keep only specs whose :addon/id ∈ :selected-ids.
   Returns {:specs [MountSpec] :dropped {addon-id reason} :unjoinable [lib-sym]
    :selected-absent #{addon-id} :config-by-id {addon-id config}}. :selected-absent
   are plug-selected ids with no discovered spec (reported, not an error)."
  [discovered-specs
   {:keys [dropped-ids selected-ids dropped-reasons config-by-id unjoinable]}
   {:keys [strict-select]}]
  (let [discovered-ids (into #{} (map :addon/id) discovered-specs)
        keep?       (fn [id] (if strict-select
                               (contains? selected-ids id)
                               (or (contains? selected-ids id)
                                   (not (contains? dropped-ids id)))))
        drop-reason (fn [id] (or (get dropped-reasons id)
                                 (if strict-select :not-selected :dropped)))
        {:keys [specs dropped]}
        (reduce (fn [acc spec]
                  (let [id (:addon/id spec)]
                    (if (keep? id)
                      (update acc :specs conj spec)
                      (assoc-in acc [:dropped id] (drop-reason id)))))
                {:specs [] :dropped {}} discovered-specs)]
    {:specs            specs
     :dropped          dropped
     :unjoinable       (vec (or unjoinable []))
     :selected-absent  (set/difference (set selected-ids) discovered-ids)
     :config-by-id     (or config-by-id {})}))

(defn compose-config-resolver
  "Wrap base-resolver so per-addon build.edn :config overrides merge ON TOP of
   the base (e.g. hive-di) config. base-resolver defaults to
   port/resolve-config-default. Returns (fn [spec] -> config-map)."
  ([config-by-id] (compose-config-resolver port/resolve-config-default config-by-id))
  ([base-resolver config-by-id]
   (fn [spec]
     (merge (base-resolver spec)
            (get config-by-id (:addon/id spec) {})))))

;; =============================================================================
;; Effectful shell
;; =============================================================================

(defn- solve-opts
  "Project only present solve keys — never pass nil, which would clobber solve's
   :or defaults."
  [{:keys [rules fail-closed-cycles]}]
  (cond-> {}
    (some? rules)              (assoc :rules rules)
    (some? fail-closed-cycles) (assoc :fail-closed-cycles fail-closed-cycles)))

(defn- solve-error? [plan]
  (and (map? plan) (contains? plan :error)))

(defn- layers-with-plugs? [layers]
  (boolean (some #(seq (:iaddon/plugs (:config %))) layers)))

(defn compose-plan
  "Pure composition (no mount effects): select discovered specs through the plug
   layers, then solve the mount order. Returns
   (r/ok {:selected-ids #{addon-id} :dropped {addon-id reason} :unjoinable [lib]
          :selected-absent #{addon-id} :config-by-id {addon-id config} :plan MountPlan})
   or the (r/err ...) from plug resolution / a fail-closed solve. When no layer
   carries :iaddon/plugs every discovered spec is kept (back-compat)."
  ([discovered-specs layers] (compose-plan discovered-specs layers {}))
  ([discovered-specs layers {:keys [profile strict-select] :as opts}]
   (if-not (layers-with-plugs? layers)
     (let [plan (solve/solve discovered-specs (solve-opts opts))]
       (if (solve-error? plan)
         plan
         (r/ok {:selected-ids    (into #{} (map :addon/id) discovered-specs)
                :dropped         {}
                :unjoinable      []
                :selected-absent #{}
                :config-by-id    {}
                :plan            plan})))
     (r/let-ok [presolved (plug/resolve-config layers {:profile profile})]
       (let [idx (plug-index presolved)
             {:keys [specs dropped unjoinable selected-absent config-by-id]}
             (select-specs discovered-specs idx {:strict-select strict-select})
             plan (solve/solve specs (solve-opts opts))]
         (if (solve-error? plan)
           plan
           (r/ok {:selected-ids    (into #{} (map :addon/id) specs)
                  :dropped         dropped
                  :unjoinable      unjoinable
                  :selected-absent selected-absent
                  :config-by-id    config-by-id
                  :plan            plan})))))))

(defn compose!
  "Full composition: compose-plan then mount! the plan into host, threading
   per-addon build.edn :config over the base resolve-config. opts:
   {:profile :rules :fail-closed-cycles :strict-select
    :resolve-config (base, default port/resolve-config-default)}. Returns
   (r/ok {<compose-plan keys> :report MountReport}) or the (r/err ...) from
   compose-plan. Mount failures are recorded in :report (graceful degrade)."
  ([discovered-specs layers host] (compose! discovered-specs layers host {}))
  ([discovered-specs layers host
    {:keys [resolve-config] :or {resolve-config port/resolve-config-default} :as opts}]
   (r/let-ok [composed (compose-plan discovered-specs layers
                                     (select-keys opts [:profile :rules
                                                        :fail-closed-cycles :strict-select]))]
     (let [{:keys [plan config-by-id]} composed
           report (boundary/mount! plan host
                                   {:resolve-config (compose-config-resolver resolve-config config-by-id)})]
       (r/ok (assoc composed :report report))))))

(defn- parse-validate-layer
  "Parse content as EDN + validate against IaddonConfig →
   {:layer {:id :config}} | {:error {:path :error}} | {:missing? true} (empty/nil)."
  [path content]
  (let [parsed (r/rescue ::parse-error (edn/read-string content))]
    (cond
      (= parsed ::parse-error) {:error {:path path :error "invalid EDN"}}
      (nil? parsed)            {:missing? true}
      :else
      (let [res (plug-schema/validate* plug-schema/IaddonConfig parsed :iaddon/schema-violation)]
        (if (r/ok? res)
          {:layer {:id path :config parsed}}
          {:error {:path path :error (dissoc res :error)}})))))

(defn- read-one-layer
  "Resolve one layer path to {:layer {:id :config}} | {:error {:path :error}} |
   {:missing? true}. Filesystem file first, then classpath resource. A path that
   resolves to nothing is missing (skipped); a resolved-but-unreadable source is
   an error, not a skip. Never throws."
  [classloader path]
  (if (.exists (io/file path))
    (let [content (r/rescue ::read-error (slurp (io/file path)))]
      (if (= content ::read-error)
        {:error {:path path :error "unreadable file"}}
        (parse-validate-layer path content)))
    (if-let [u (if classloader (io/resource path classloader) (io/resource path))]
      (let [content (r/rescue ::read-error (slurp u))]
        (if (= content ::read-error)
          {:error {:path path :error "unreadable resource"}}
          (parse-validate-layer path content)))
      {:missing? true})))

(defn read-layers
  "Read ordered layer paths (lowest precedence first) into plug layers. Returns
   {:layers [{:id path :config IaddonConfig}] :errors [{:path :error}]}. A
   missing path is SKIPPED (not an error); a malformed/invalid one → :errors.
   Never throws."
  ([paths] (read-layers paths nil))
  ([paths classloader]
   (reduce (fn [acc path]
             (let [{:keys [layer error]} (read-one-layer classloader path)]
               (cond
                 layer (update acc :layers conj layer)
                 error (update acc :errors conj error)
                 :else acc)))
           {:layers [] :errors []}
           (or paths []))))

(defn compose-classpath!
  "One-call host entry: discover MountSpecs on the classpath + read plug layers
   from :layer-paths + compose! into host. Returns (r/ok {<compose! keys>
   :discovery-errors [..] :layer-errors [..]}) or the (r/err ...) from compose!.
   :layer-paths defaults to [] ⇒ mount every discovered spec (≡ mount-classpath!).
   opts also threads :profile :rules :fail-closed-cycles :strict-select
   :resolve-config."
  ([host] (compose-classpath! host {}))
  ([host {:keys [layer-paths] :as opts}]
   (let [{:keys [specs errors]}            (boundary/discover-specs)
         {:keys [layers] layer-errors :errors} (read-layers (or layer-paths []))
         result (compose! specs layers host (dissoc opts :layer-paths))]
     (if (r/ok? result)
       (r/ok (cond-> (:ok result)
               (seq errors)       (assoc :discovery-errors errors)
               (seq layer-errors) (assoc :layer-errors layer-errors)))
       result))))
