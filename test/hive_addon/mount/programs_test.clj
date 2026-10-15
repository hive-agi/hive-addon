(ns hive-addon.mount.programs-test
  "Trifecta for declarative program requirements and boundary previews. Resolver
   doubles are injected functions, so tests never depend on the host PATH."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.port :as port]
            [hive-addon.mount.schema :as ms]
            [hive-addon.mount.solve :as solve]
            [hive-test.trifecta :as tri]
            [clojure.set :as set]))

(def ^:private fixture-spec
  {:addon/id "program-probe" :addon/type :native
   :addon/init-ns "hive-addon.mount.programs-test"
   :addon/init-fn "make-addon"
   :addon/requires-programs #{"git" "rg"}})

(def ^:private real-resolve boundary/resolve-programs)

(tri/deftrifecta program-resolution
  hive-addon.mount.boundary/resolve-programs
  {:golden-path "test/golden/hive-addon/program-resolution.edn"
   :apply? true
   :cases {:all-present [fixture-spec {"git" "/bin/git" "rg" "/bin/rg"}]
           :partial [fixture-spec {"git" "/bin/git"}]
           :no-resolver [fixture-spec nil]
           :no-requirements [(dissoc fixture-spec :addon/requires-programs) nil]}
   :mutations
   [["pretends-everything-is-installed" (fn [sp resolver]
                                       (assoc (real-resolve sp resolver)
                                              :programs/missing #{}))]
    ["ignores-resolver" (fn [sp _] (real-resolve sp nil))]]})

(defspec resolved-and-missing-partition-declared-programs 100
  (prop/for-all [names (gen/set gen/string-alphanumeric {:max-elements 6})
                 present (gen/set gen/string-alphanumeric {:max-elements 6})]
    (let [result (boundary/resolve-programs {:addon/requires-programs names}
                                            #(when (contains? present %) (str "/bin/" %)))]
      (and (= names (set (keys (:programs/resolved result))))
           (= (set/difference names present) (:programs/missing result))
           (every? (fn [[name path]] (= (some? path) (contains? present name)))
                   (:programs/resolved result))))))

(defn make-addon [_]
  (throw (ex-info "constructor must not run for a missing program" {})))

(deftest manifest-validates-program-names
  (is (ms/validate ms/MountSpec fixture-spec))
  (is (not (ms/validate ms/MountSpec
                        (assoc fixture-spec :addon/requires-programs #{""}))))
  (is (not (ms/validate ms/MountSpec
                        (assoc fixture-spec :addon/requires-programs #{:git})))))

(deftest dry-run-and-mount-report-missing-prerequisites
  (let [plan (solve/solve [fixture-spec])
        host (port/atom-mount-host)
        opts {:resolve-program {"git" "/bin/git"}}
        preview (boundary/dry-run plan host opts)
        actual (boundary/mount! plan host opts)
        p (first (:mounted preview))
        a (first (:mounted actual))]
    (is (= :programs (:phase p)))
    (is (= :programs (:phase a)))
    (is (= #{"rg"} (get-in p [:programs :programs/missing])))
    (is (= {"git" "/bin/git" "rg" nil}
           (get-in p [:programs :programs/resolved])))
    (is (false? (:ok? preview)))
    (is (false? (:ok? actual)))
    (is (= #{"program-probe"} (:skipped actual)))
    (is (ms/validate ms/MountReport preview))
    (is (ms/validate ms/MountReport actual))))

(deftest undeclared-programs-do-not-need-a-resolver
  (let [plan (solve/solve [(dissoc fixture-spec :addon/requires-programs)])
        preview (boundary/dry-run plan (port/atom-mount-host))]
    (is (:ok? preview))
    (is (= {} (get-in preview [:mounted 0 :programs :programs/resolved])))))
