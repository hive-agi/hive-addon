(ns hive-addon.mount.boundary
  "Effectful boundary of the addon mounter — all IO and var resolution, injected
   through the IMountHost port and an optional config resolver.

   Collect: discover-specs scans the classpath for META-INF/hive-addons/*.edn.
   Promote: parse-spec turns an EDN string into a validated MountSpec Result.
   Pipeline is hive-addon.mount.solve (pure, elsewhere).
   Boundary: mount!/dry-run/teardown! resolve constructors, inject already-mounted
   sibling instances into each dependent's config (DIP), and drive the host.

   mount! GRACEFULLY DEGRADES: a spec that fails at any step is recorded in the
   MountReport and the loop CONTINUES; already-mounted addons are NEVER torn down
   on a mid-DAG failure. teardown! shuts down in reverse mount order and always
   reports :teardown/data-preserved? true (shutdown! never deletes data)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-addon.mount.port :as port]
            [hive-addon.mount.schema :as ms]
            [hive-addon.mount.solve :as solve]
            [hive-addon.protocol :as proto]
            [hive-dsl.result :as r])
  (:import [java.util.jar JarFile]
           [java.net URL]
           [java.io File]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Collect — classpath discovery of META-INF/hive-addons/*.edn
;; =============================================================================

(def ^:private manifest-resource-path
  "Classpath resource dir addons drop their mount manifests into."
  "META-INF/hive-addons")

(defn- list-edn-files-from-dir
  "List .edn file URLs under a file: directory URL."
  [^File dir]
  (when (.isDirectory dir)
    (->> (.listFiles dir)
         (filter #(str/ends-with? (.getName ^File %) ".edn"))
         (mapv #(.toURL ^File %)))))

(defn- list-edn-files-from-jar
  "List .edn entry URLs under META-INF/hive-addons/ inside a JAR."
  [^URL jar-url]
  (r/rescue []
    (let [jar-path (-> (.getPath jar-url)
                       (str/replace #"^file:" "")
                       (str/replace #"!.*$" ""))
          jar-file (JarFile. jar-path)
          prefix   (str manifest-resource-path "/")
          entries  (->> (enumeration-seq (.entries jar-file))
                        (filter (fn [e]
                                  (let [n (.getName e)]
                                    (and (str/starts-with? n prefix)
                                         (str/ends-with? n ".edn")
                                         (not (.isDirectory e))))))
                        (mapv (fn [e]
                                (URL. (str "jar:file:" jar-path "!/" (.getName e))))))]
      (.close jar-file)
      entries)))

(defn- discover-manifest-urls
  "Scan the classpath (file: dirs + jar: entries) for all
   META-INF/hive-addons/*.edn resources. Returns a vec of URLs."
  [^ClassLoader classloader]
  (let [urls (enumeration-seq (.getResources classloader manifest-resource-path))]
    (into []
          (mapcat
           (fn [^URL url]
             (case (.getProtocol url)
               "file" (list-edn-files-from-dir (io/file url))
               "jar"  (list-edn-files-from-jar url)
               [])))
          urls)))

;; =============================================================================
;; Promote — EDN string -> validated MountSpec Result
;; =============================================================================

(defn parse-spec
  "Parse an EDN manifest string into a validated MountSpec.
   (r/ok MountSpec) on success; (r/err :mount/spec-invalid {:explanation ..})
   on a read error or schema violation."
  [edn-string]
  (let [parsed (r/try-effect* :mount/spec-invalid (edn/read-string edn-string))]
    (if (r/err? parsed)
      parsed
      (ms/validate* ms/MountSpec (:ok parsed) :mount/spec-invalid))))

(defn discover-specs
  "Scan the classpath for META-INF/hive-addons/*.edn and parse each into a
   MountSpec. Returns {:specs [MountSpec] :errors [{:url .. :errors ..}]}.
   Result-guarded: a bad file becomes an :errors entry, never aborts the scan."
  ([] (discover-specs (.getContextClassLoader (Thread/currentThread))))
  ([^ClassLoader classloader]
   (reduce
    (fn [acc url]
      (let [content (r/rescue nil (slurp url))]
        (if (nil? content)
          (update acc :errors conj {:url (str url) :errors ["unreadable manifest resource"]})
          (let [res (parse-spec content)]
            (if (r/ok? res)
              (update acc :specs conj (:ok res))
              (update acc :errors conj {:url (str url) :errors (:explanation res)}))))))
    {:specs [] :errors []}
    (discover-manifest-urls classloader))))

;; =============================================================================
;; Boundary helpers — constructor resolution + sibling-instance injection
;; =============================================================================

(defn resolve-constructor
  "Resolve a MountSpec's :addon/init-ns / :addon/init-fn into a ctor fn via
   requiring-resolve, or nil if it cannot be resolved. Never throws."
  [spec]
  (r/rescue nil
    (some-> (requiring-resolve
             (symbol (:addon/init-ns spec) (:addon/init-fn spec)))
            var-get)))

(defn- dependency-ids
  "The ids `spec` depends on within `all-specs`: hard :addon/dependencies plus
   every peer whose :addon/capabilities satisfy a :addon/requires-capabilities."
  [spec all-specs]
  (let [hard (set (:addon/dependencies spec #{}))
        reqs (set (:addon/requires-capabilities spec #{}))
        cap-providers (into #{}
                            (comp
                             (filter (fn [p]
                                       (and (not= (:addon/id p) (:addon/id spec))
                                            (some reqs (:addon/capabilities p #{})))))
                             (map :addon/id))
                            all-specs)]
    (into hard cap-providers)))

(defn- inject-dependencies
  "Assoc :mount/dependencies {dep-id instance ...} into config for every dep of
   `spec` already registered in `host` — the DIP sibling injection."
  [config host spec all-specs]
  (let [deps (into {}
                   (keep (fn [dep-id]
                           (when-let [inst (port/registered host dep-id)]
                             [dep-id inst])))
                   (dependency-ids spec all-specs))]
    (assoc config :mount/dependencies deps)))

(defn- mount-result
  [id success? phase & {:keys [errors already-initialized?]}]
  (cond-> {:addon/id id :success? success? :phase phase}
    (seq errors)                (assoc :errors (vec errors))
    (some? already-initialized?) (assoc :already-initialized? already-initialized?)))

;; =============================================================================
;; mount! — drive the plan through the host, graceful degrade
;; =============================================================================

(defn- mount-one
  "Attempt to mount a single spec into host. Returns a MountResult. Never
   throws — every failure is folded into the result (graceful degrade)."
  [spec host all-specs resolve-config]
  (let [id   (:addon/id spec)
        ctor (resolve-constructor spec)]
    (if (nil? ctor)
      (mount-result id false :resolved :errors ["constructor could not be resolved"])
      (let [cfg (r/try-effect (inject-dependencies (resolve-config spec) host spec all-specs))]
        (if (r/err? cfg)
          (mount-result id false :config :errors [(:message cfg)])
          (let [config   (:ok cfg)
                instance (r/rescue nil (ctor config))]
            (cond
              (nil? instance)
              (mount-result id false :failed :errors ["constructor returned nil or threw"])

              (not (proto/addon? instance))
              (mount-result id false :failed :errors ["constructor did not return an IAddon"])

              :else
              (let [reg (r/try-effect (port/register! host instance))]
                (if (r/err? reg)
                  (mount-result id false :registered :errors [(:message reg)])
                  (let [init (r/try-effect (port/init! host id config))]
                    (if (r/err? init)
                      (mount-result id false :initialized :errors [(:message init)])
                      (let [ir (:ok init)]
                        (mount-result id (boolean (:success? ir)) :initialized
                                      :errors (:errors ir)
                                      :already-initialized? (:already-initialized? ir))))))))))))))

(defn mount!
  "Mount every spec in (plan :ordered) into host, in order. Returns a MountReport.
   For each spec: resolve ctor, build config via resolve-config then inject
   already-mounted sibling instances under :mount/dependencies, construct, verify
   IAddon, register!, init!. Any failure is recorded and the loop CONTINUES —
   already-mounted addons are NEVER torn down (graceful degrade). :ok? is true
   only when every attempted spec succeeded.

   opts: {:resolve-config (fn [spec] -> config-map)  (default resolve-config-default)}"
  ([plan host] (mount! plan host {}))
  ([plan host {:keys [resolve-config] :or {resolve-config port/resolve-config-default}}]
   (let [ordered (:ordered plan)
         results (mapv #(mount-one % host ordered resolve-config) ordered)]
     {:mounted results
      :order   (mapv :addon/id ordered)
      :skipped (into #{} (comp (remove :success?) (map :addon/id)) results)
      :ok?     (every? :success? results)})))

;; =============================================================================
;; dry-run — same shape, NO effects (golden-replay parity)
;; =============================================================================

(defn dry-run
  "Compute the MountReport that mounting would produce WITHOUT any effects:
   no construction, registration, or initialization. Resolves each ctor
   read-only and marks it :phase :skipped :success? true when resolvable, else
   :phase :resolved :success? false. Golden-replay parity: for an all-success
   plan, dry-run :order and :mounted ids equal mount! :order and :mounted ids."
  ([plan host] (dry-run plan host {}))
  ([plan _host _opts]
   (let [ordered (:ordered plan)
         results (mapv (fn [spec]
                         (let [id (:addon/id spec)]
                           (if (resolve-constructor spec)
                             (mount-result id true :skipped)
                             (mount-result id false :resolved
                                           :errors ["constructor could not be resolved"]))))
                       ordered)]
     {:mounted results
      :order   (mapv :addon/id ordered)
      :skipped (into #{} (comp (remove :success?) (map :addon/id)) results)
      :ok?     (every? :success? results)})))

;; =============================================================================
;; teardown! — shutdown in reverse mount order, no-nuke
;; =============================================================================

(defn teardown!
  "Shut down addon-ids in REVERSE order via (port/shutdown! host id). Returns a
   TeardownReport. :teardown/data-preserved? is always true (shutdown! never
   deletes data); a shutdown that throws is recorded in :errors but the flag
   stays true — we did not delete."
  [host addon-ids]
  (let [order  (vec (reverse addon-ids))
        errors (into []
                     (keep (fn [id]
                             (let [res (r/try-effect (port/shutdown! host id))]
                               (when (r/err? res)
                                 (str id ": " (:message res))))))
                     order)]
    (cond-> {:torn-down order
             :teardown/data-preserved? true}
      (seq errors) (assoc :errors errors))))
