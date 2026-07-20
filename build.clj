(ns build
  "Build + Clojars/Maven deploy for hive-addon.

     clojure -T:build jar      ;; build target/hive-addon-<VERSION>.jar + pom
     clojure -T:build deploy   ;; build then deploy to Clojars

   Deploy reads CLOJARS_USERNAME + CLOJARS_PASSWORD (a Clojars deploy token)
   from the environment. Version is read from the VERSION file (single source)."
  (:require [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'io.github.hive-agi/hive-addon)
(def version (str/trim (slurp "VERSION")))
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn- basis [] (b/create-basis {:project "deps.edn"}))

(defn clean [_] (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis (basis)
                :src-dirs ["src"]
                :scm {:url "https://github.com/hive-agi/hive-addon"
                      :connection "scm:git:git://github.com/hive-agi/hive-addon.git"
                      :developerConnection "scm:git:ssh://git@github.com/hive-agi/hive-addon.git"
                      :tag (str "v" version)}
                :pom-data [[:licenses
                            [:license
                             [:name "MIT"]
                             [:url "https://opensource.org/licenses/MIT"]]]]})
  (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
  (b/jar {:class-dir class-dir :jar-file jar-file})
  (println "Built" jar-file))

(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :sign-releases? false
              :artifact (b/resolve-path jar-file)
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})})
  (println "Deployed" (str lib) version "to Clojars"))

(defn- already-published-private?
  "True if this exact lib+version pom is already in the private Gitea registry.
   Authed HEAD — private-org reads require credentials."
  [url username token]
  (let [[grp art] (str/split (str lib) #"/")
        pom-url (format "%s/%s/%s/%s/%s-%s.pom"
                        url (str/replace grp "." "/") art version art version)
        auth (str "Basic " (.encodeToString (java.util.Base64/getEncoder)
                                            (.getBytes (str username ":" token))))]
    (try
      (let [conn (doto ^java.net.HttpURLConnection (.openConnection (java.net.URL. pom-url))
                   (.setRequestMethod "HEAD")
                   (.setRequestProperty "Authorization" auth)
                   (.setConnectTimeout 10000)
                   (.setReadTimeout 10000))]
        (= 200 (.getResponseCode conn)))
      (catch Throwable _ false))))

(defn deploy-private
  "Build + deploy to the private Gitea Maven registry (hive-agi org).
   Env: GITEA_MAVEN_TOKEN (required, non-blank), GITEA_MAVEN_USERNAME (default buddhilw),
   GITEA_MAVEN_URL (default https://gitea.hive-mcp.com/api/packages/hive-agi/maven).
   No-ops when this version already exists in the registry (idempotent)."
  [_]
  (let [env (fn [k fallback]
              (let [v (System/getenv k)]
                (if (str/blank? v) fallback v)))
        url (env "GITEA_MAVEN_URL" "https://gitea.hive-mcp.com/api/packages/hive-agi/maven")
        username (env "GITEA_MAVEN_USERNAME" "buddhilw")
        token (System/getenv "GITEA_MAVEN_TOKEN")]
    (when (str/blank? token)
      (throw (ex-info "GITEA_MAVEN_TOKEN is required (non-blank)" {:env "GITEA_MAVEN_TOKEN"})))
    (if (already-published-private? url username token)
      (println "Skip:" (str lib) version "already in private registry — bump VERSION to release.")
      (do
        (jar nil)
        (dd/deploy {:installer  :remote
                    :artifact   (b/resolve-path jar-file)
                    :pom-file   (b/pom-path {:lib lib :class-dir class-dir})
                    :repository {"gitea" {:url url :username username :password token}}})
        (println "Deployed" (str lib) version "to" url)))))
