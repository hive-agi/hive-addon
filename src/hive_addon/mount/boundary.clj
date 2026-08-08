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
            [hive-dsl.result :as r]
            [hive-addon.mount.entitlement :as ent])
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

(defn- root-cause
  [^Throwable error]
  (loop [cause error]
    (if-let [next-cause (ex-cause cause)]
      (recur next-cause)
      cause)))

(defn resolve-constructor
  "Resolve a MountSpec constructor without erasing load failures.

   Returns tagged resolution data. `:absent` means no such var; `:failed`
   means requiring the constructor namespace threw (including compiler/load
   order failures); `:invalid` means the var exists but is not callable.
   Never throws ordinary Exceptions."
  [spec]
  (let [sym (symbol (:addon/init-ns spec) (:addon/init-fn spec))]
    (try
      (if-let [resolved (requiring-resolve sym)]
        (let [ctor (var-get resolved)]
          (if (ifn? ctor)
            {:constructor/status :resolved
             :constructor/symbol (str sym)
             :constructor ctor}
            {:constructor/status :invalid
             :constructor/symbol (str sym)
             :constructor/error (str "constructor var is not callable: " sym)}))
        {:constructor/status :absent
         :constructor/symbol (str sym)
         :constructor/error (str "constructor var is absent: " sym)})
      (catch Exception error
        (let [root (root-cause error)]
          {:constructor/status :failed
           :constructor/symbol (str sym)
           :constructor/error (str "constructor namespace failed to load: " sym
                                   " — " (or (ex-message error)
                                             (.getName (class error))))
           :constructor/exception (.getName (class error))
           :constructor/cause (.getName (class root))
           :constructor/cause-message (or (ex-message root)
                                          (.getName (class root)))})))))

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
  [id success? phase & {:keys [errors already-initialized? init-attempts
                               constructor-resolution]}]
  (cond-> {:addon/id id :success? success? :phase phase}
    (seq errors)                (assoc :errors (vec errors))
    (some? already-initialized?) (assoc :already-initialized? already-initialized?)
    (some? init-attempts)        (assoc :init-attempts init-attempts)
    (map? constructor-resolution) (merge (dissoc constructor-resolution :constructor))))

(def default-init-retry
  "Default bounded initializer retry policy. :max-attempts counts the first call."
  {:max-attempts 1
   :initial-delay-ms 100
   :max-delay-ms 5000
   :backoff-factor 2})

(defn- retry-policy
  [spec override]
  (let [{:keys [max-attempts initial-delay-ms max-delay-ms backoff-factor] :as policy}
        (merge default-init-retry (:addon/init-retry spec) override)]
    (when-not (pos-int? max-attempts)
      (throw (ex-info ":max-attempts must be a positive integer" {:policy policy})))
    (when-not (and (int? initial-delay-ms) (not (neg? initial-delay-ms)))
      (throw (ex-info ":initial-delay-ms must be a non-negative integer" {:policy policy})))
    (when-not (and (int? max-delay-ms) (not (neg? max-delay-ms)))
      (throw (ex-info ":max-delay-ms must be a non-negative integer" {:policy policy})))
    (when-not (and (number? backoff-factor) (<= 1 backoff-factor))
      (throw (ex-info ":backoff-factor must be a number >= 1" {:policy policy})))
    policy))

(defn- retry-delay-ms
  [{:keys [initial-delay-ms max-delay-ms backoff-factor]} attempt]
  (long
   (min max-delay-ms
        (* initial-delay-ms
           (Math/pow (double backoff-factor) (double (dec attempt)))))))

(defn- emit!
  [on-event event]
  (when on-event
    (r/rescue nil (on-event event))))

(defn- init-attempt
  [host id config]
  (let [effect (r/try-effect (port/init! host id config))]
    (if (r/err? effect)
      {:success? false :errors [(:message effect)]}
      (let [result (:ok effect)]
        (if (map? result)
          (if (:success? result)
            result
            (update result :errors
                    #(if (seq %) (vec %) ["initializer reported failure"])))
          {:success? false
           :errors ["initializer returned a non-map result"]})))))

(defn- retry-init!
  [host id config policy on-event sleep-fn]
  (loop [attempt 1]
    (emit! on-event {:event :mount/init-attempt
                     :level :debug
                     :addon/id id
                     :attempt attempt
                     :max-attempts (:max-attempts policy)})
    (let [result (init-attempt host id config)
          result (assoc result :init-attempts attempt)]
      (cond
        (:success? result)
        (do
          (emit! on-event {:event :mount/init-succeeded
                           :level :info
                           :addon/id id
                           :attempt attempt
                           :max-attempts (:max-attempts policy)})
          result)

        (>= attempt (:max-attempts policy))
        (do
          (emit! on-event {:event :mount/init-failed
                           :level :error
                           :addon/id id
                           :attempt attempt
                           :max-attempts (:max-attempts policy)
                           :errors (vec (:errors result))})
          result)

        :else
        (let [delay-ms (retry-delay-ms policy attempt)]
          (emit! on-event {:event :mount/init-retry
                           :level :warn
                           :addon/id id
                           :attempt attempt
                           :next-attempt (inc attempt)
                           :max-attempts (:max-attempts policy)
                           :delay-ms delay-ms
                           :errors (vec (:errors result))})
          (let [sleep-result (r/try-effect (sleep-fn delay-ms))]
            (if (r/err? sleep-result)
              (let [failed {:success? false
                            :errors [(str "retry delay failed: " (:message sleep-result))]
                            :init-attempts attempt}]
                (emit! on-event {:event :mount/init-failed
                                 :level :error
                                 :addon/id id
                                 :attempt attempt
                                 :max-attempts (:max-attempts policy)
                                 :errors (:errors failed)})
                failed)
              (recur (inc attempt)))))))))

;; =============================================================================
;; mount! — drive the plan through the host, graceful degrade
;; =============================================================================

(defn- mount-one
  "Attempt to mount a single spec into host. Returns a MountResult. Never
   throws — every failure is folded into the result (graceful degrade).

   The licence gate runs FIRST: a refused spec never has its constructor
   namespace loaded, so unlicensed code is not merely unused but unread."
  [spec host all-specs resolve-config init-retry on-event sleep-fn gate]
  (let [id (:addon/id spec)]
    (if-let [reason (ent/permit gate spec)]
      (do
        (emit! on-event {:event :mount/entitlement-refused
                         :level :warn
                         :addon/id id
                         :deny/reason reason})
        (assoc (mount-result id false :entitlement
                             :errors [(str "licence gate refused: " (symbol reason))])
               :deny/reason reason))
      (let [resolution (resolve-constructor spec)
            ctor       (:constructor resolution)]
        (if-not (= :resolved (:constructor/status resolution))
          (mount-result id false :resolved
                        :errors [(:constructor/error resolution)]
                        :constructor-resolution resolution)
          (let [cfg (r/try-effect
                     {:config (inject-dependencies (resolve-config spec) host spec all-specs)
                      :retry-policy (retry-policy spec init-retry)})]
            (if (r/err? cfg)
              (mount-result id false :config :errors [(:message cfg)])
              (let [{:keys [config retry-policy]} (:ok cfg)
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
                      (let [ir (retry-init! host id config retry-policy on-event sleep-fn)]
                        (mount-result id (boolean (:success? ir)) :initialized
                                      :errors (:errors ir)
                                      :already-initialized? (:already-initialized? ir)
                                      :init-attempts (:init-attempts ir))))))))))))))

(defn mount!
  "Mount every spec in (plan :ordered) into host, in order. Returns a MountReport.
   For each spec: check the licence gate, resolve ctor, build config via
   resolve-config then inject already-mounted sibling instances under
   :mount/dependencies, construct, verify IAddon, register!, init!. Any failure
   is recorded and the loop CONTINUES — already-mounted addons are NEVER torn
   down (graceful degrade). :ok? is true only when every attempted spec
   succeeded.

   opts: {:resolve-config (fn [spec] -> config-map)  (default resolve-config-default)
          :init-retry    retry-policy override
          :license-gate  ILicenseGate or (fn [spec] -> nil | reason)
                         (default: the installed gate)
          :on-event      (fn [event-map])
          :sleep-fn      (fn [milliseconds])}."
  ([plan host] (mount! plan host {}))
  ([plan host {:keys [resolve-config init-retry on-event sleep-fn license-gate]
               :or {resolve-config port/resolve-config-default
                    init-retry {}
                    on-event (constantly nil)
                    sleep-fn (fn [ms] (Thread/sleep (long ms)))}}]
   (let [gate    (or license-gate (ent/installed-gate))
         ordered (:ordered plan)
         results (mapv #(mount-one % host ordered resolve-config
                                   init-retry on-event sleep-fn gate)
                       ordered)]
     {:mounted results
      :order   (mapv :addon/id ordered)
      :skipped (into #{} (comp (remove :success?) (map :addon/id)) results)
      :ok?     (every? :success? results)})))

;; =============================================================================
;; dry-run — same shape, NO effects (golden-replay parity)
;; =============================================================================

(defn dry-run
  "Compute the MountReport that mounting would produce WITHOUT any effects:
   no construction, registration, or initialization. Applies the licence gate
   and resolves each ctor read-only, marking a spec :phase :skipped :success?
   true when it would mount, :phase :entitlement when the gate refuses, else
   :phase :resolved :success? false. Golden-replay parity: for an all-success
   plan, dry-run :order and :mounted ids equal mount! :order and :mounted ids."
  ([plan host] (dry-run plan host {}))
  ([plan _host {:keys [license-gate]}]
   (let [gate    (or license-gate (ent/installed-gate))
         ordered (:ordered plan)
         results (mapv (fn [spec]
                         (let [id (:addon/id spec)]
                           (if-let [reason (ent/permit gate spec)]
                             (assoc (mount-result id false :entitlement
                                                  :errors [(str "licence gate refused: " (symbol reason))])
                                    :deny/reason reason)
                             (let [resolution (resolve-constructor spec)]
                               (if (= :resolved (:constructor/status resolution))
                                 (mount-result id true :skipped)
                                 (mount-result id false :resolved
                                               :errors [(:constructor/error resolution)]
                                               :constructor-resolution resolution))))))
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
