(ns hive-addon.hot.source
  "Where an addon's constructor namespace physically lives — the Collect stratum
   of the hot-reload bridge.

   An addon wired as a `:local/root` dep (typically through an untracked
   local.deps.edn override) puts its `src` on the classpath as a real DIRECTORY:
   the bytes on disk can change, so clj-reload can watch and reload it. The same
   addon consumed as an `:mvn/version` coordinate arrives inside a JAR, whose
   bytes cannot change without a restart — hot-reload of it is not merely
   unsupported, it is meaningless.

   That distinction is the input to strategy selection, so it is resolved here as
   DATA (an AddonSource) rather than guessed at the reload site.

   Pure apart from classpath lookup; never throws — an unresolvable namespace
   becomes {:hot/source-kind :absent}."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :as r])
  (:import [java.net URL]
           [java.io File]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; ns -> classpath resource paths
;; =============================================================================

(def ^:private source-extensions
  "Extensions probed for a namespace's source, in preference order. `.clj` wins
   over `.cljc` because that is the load order the Clojure runtime itself uses."
  [".clj" ".cljc"])

(defn ns->base-path
  "Munge a namespace name into its classpath path stem (no extension).
   `hive-carto.carto-init` -> `hive_carto/carto_init`."
  [ns-str]
  (-> (str ns-str)
      (str/replace "-" "_")
      (str/replace "." "/")))

(defn ns->resource-paths
  "Candidate classpath resource paths for a namespace, in load order."
  [ns-str]
  (let [base (ns->base-path ns-str)]
    (mapv #(str base %) source-extensions)))

;; =============================================================================
;; URL -> source kind
;; =============================================================================

(defn- strip-suffix
  "Drop `suffix` from the end of `s`, or nil when it is not a suffix."
  [^String s ^String suffix]
  (when (str/ends-with? s suffix)
    (subs s 0 (- (count s) (count suffix)))))

(defn- classpath-root
  "The classpath ROOT directory that contains `resource-path` at `url` — i.e. the
   directory a consumer would hand clj-reload as a `:dirs` entry. Peeling the
   resource path off the file path is what turns
   `…/hive-carto/src/hive_carto/carto_init.clj` into `…/hive-carto/src`."
  [^URL url ^String resource-path]
  (r/rescue nil
    (let [path (.getPath (.toURI url))
          root (strip-suffix path (str File/separator resource-path))
          root (or root (strip-suffix path (str "/" resource-path)))]
      (when root
        (.getCanonicalPath (io/file root))))))

(defn- classify-url
  "Classify a resolved source URL into a SourceKind. Anything that is not a
   plain `file:` URL is treated as non-reloadable — a jar, and by extension any
   exotic protocol, cannot have its bytes changed underneath a running JVM."
  [^URL url]
  (if (= "file" (.getProtocol url)) :directory :jar))

;; =============================================================================
;; Resolution
;; =============================================================================

(defn resolve-source
  "Resolve where `ns-str`'s source lives on the classpath.

   Returns an AddonSource-shaped map WITHOUT :addon/id (the caller stamps that):
     {:addon/init-ns   ns-str
      :hot/source-kind :directory | :jar | :absent
      :hot/reloadable? bool
      :hot/source-dir  <classpath root, :directory only>
      :hot/source-url  <resolved URL string, when found>}

   Never throws — a namespace whose source is absent from the classpath (AOT-only
   or dynamically generated) resolves to :absent, which is a strategy input, not
   an error."
  [ns-str]
  (let [probe (->> (ns->resource-paths ns-str)
                   (keep (fn [path]
                           (when-let [url (io/resource path)]
                             {:path path :url url})))
                   first)]
    (if-not probe
      {:addon/init-ns   (str ns-str)
       :hot/source-kind :absent
       :hot/reloadable? false}
      (let [{:keys [^URL url path]} probe
            kind (classify-url url)
            dir  (when (= :directory kind) (classpath-root url path))]
        (cond-> {:addon/init-ns   (str ns-str)
                 :hot/source-kind kind
                 :hot/reloadable? (and (= :directory kind) (some? dir))
                 :hot/source-url  (str url)}
          dir (assoc :hot/source-dir dir))))))

(defn spec-source
  "Resolve the AddonSource for a MountSpec, stamped with its :addon/id."
  [spec]
  (assoc (resolve-source (:addon/init-ns spec))
         :addon/id (:addon/id spec)))

(defn sources
  "Resolve every spec's AddonSource. Returns a vector in spec order."
  [specs]
  (mapv spec-source specs))

(defn watchable-dirs
  "The set of classpath-root directories among `specs` that clj-reload can watch.

   This is what a consumer feeds hive-hot's `:dirs`: only addons wired by
   `:local/root` contribute, because only they have source on disk. Jar-backed
   and absent addons contribute nothing and are simply not watched."
  [specs]
  (into #{}
        (comp (map spec-source)
              (filter :hot/reloadable?)
              (keep :hot/source-dir))
        specs))
