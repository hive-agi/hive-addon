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
