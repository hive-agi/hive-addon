(ns hive-addon.mount.programs-host-test
  "A host's :resolve-program must survive every path from the host to
   boundary/mount! and boundary/dry-run: compose!, the hot mount driver's
   preflight, and the hot reload option folding."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.hot :as hot]
            [hive-addon.hot.mount-driver :as driver]
            [hive-addon.hot.port :as hport]
            [hive-addon.mount.compose :as compose]
            [hive-addon.mount.port :as port]
            [hive-addon.mount.solve :as solve]
            [hive-dsl.result :as r]))

(def ^:private spec
  {:addon/id "programs-host-probe" :addon/type :native
   :addon/init-ns "hive-addon.mount.programs-host-test"
   :addon/init-fn "make-addon"
   :addon/requires-programs #{"git"}})

(defn make-addon [_]
  (throw (ex-info "constructor stub" {})))

(deftest compose-threads-resolve-program-to-mount
  (let [present (compose/compose! [spec] [] (port/atom-mount-host)
                                  {:resolve-program {"git" "/bin/git"}})
        absent (compose/compose! [spec] [] (port/atom-mount-host) {})]
    (is (r/ok? present))
    (is (not= :programs (get-in present [:ok :report :mounted 0 :phase])))
    (is (= :programs (get-in absent [:ok :report :mounted 0 :phase])))))

(deftest preflight-passes-resolve-program-to-dry-run
  (let [plan (solve/solve [spec])
        d (driver/mount-driver)
        ok (hport/-preflight d plan (port/atom-mount-host) {:resolve-program {"git" "/bin/git"}})
        no (hport/-preflight d plan (port/atom-mount-host) {})]
    (is (= :skipped (get-in ok [:mounted 0 :phase])))
    (is (= :programs (get-in no [:mounted 0 :phase])))))

(deftest resolve-program-is-a-mount-opt-key
  (is (contains? hot/mount-opt-keys :resolve-program)))
