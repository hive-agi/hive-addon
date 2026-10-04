(ns hive-addon.hot.report-trifecta-test
  "Trifecta coverage for hive-addon.hot.report — the pure verdicts every
   hot-reload report is built from.

   Each mutant is a REAL regression of the claim the verdict makes:
   data-preserved? asserted true by construction (the defect this namespace
   replaced), an unclaimed release trusted, a failed id reported restored
   without evidence, a dir still in use released."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.hot.report :as report]
            [hive-addon.hot.schema :as hs]
            [hive-schemas.test :as hst]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; Captured before any mutation rebinds the var roots, so a mutant can be
;; written in terms of the real verdict without recursing into itself.
(def ^:private real-remount-outcome report/remount-outcome)

;; =============================================================================
;; data-preserved?
;; =============================================================================

(tri/deftrifecta data-preserved-verdict
  hive-addon.hot.report/data-preserved?
  {:golden-path "test/golden/hive-addon/data-preserved.edn"
   :apply? true
   :cases {:nothing-ran        [[]]
           :released-claimed   [[{:torn-down ["a"] :teardown/data-preserved? true}]]
           :released-unclaimed [[{:torn-down ["a"]}]]
           :released-denied    [[{:torn-down ["a"] :teardown/data-preserved? false}]]
           :empty-unclaimed    [[{:torn-down []}]]
           :mixed              [[{:torn-down ["a"] :teardown/data-preserved? true}
                                 {:torn-down ["b"]}]]}
   :mutations
   [;; The defect this verdict replaced: true by construction.
    ["constant-true" (fn [_] true)]
    ;; Trusts a release that made no claim.
    ["absent-means-preserved"
     (fn [outcomes] (every? #(not (false? (:teardown/data-preserved? %))) outcomes))]
    ;; Counts an outcome that released nothing against the verdict.
    ["empty-counts"
     (fn [outcomes] (every? #(true? (:teardown/data-preserved? %)) outcomes))]]})

;; =============================================================================
;; remount-outcome
;; =============================================================================

(defn- result [id ok & {:keys [restored?]}]
  (cond-> {:addon/id id :success? ok :phase (if ok :initialized :failed)}
    (some? restored?) (assoc :restored? restored?)))

(defn- mount-report [& results]
  {:mounted (vec results) :order (mapv :addon/id results)
   :skipped (into #{} (comp (remove :success?) (map :addon/id)) results)
   :ok? (every? :success? results)})

(tri/deftrifecta remount-outcome-verdict
  hive-addon.hot.report/remount-outcome
  {:golden-path "test/golden/hive-addon/remount-outcome.edn"
   :apply? true
   :cases {:all-up         [["a" "b"] (mount-report (result "a" true) (result "b" true))]
           :restored       [["a" "b"] (mount-report (result "a" false :restored? true) (result "b" true))]
           :down           [["a"] (mount-report (result "a" false))]
           :restore-failed [["a"] (mount-report (result "a" false :restored? false))]
           :never-tried    [["a" "b"] (mount-report (result "a" true))]}
   :mutations
   [;; Reports nothing down whatever happened — the silent outage.
    ["never-down"
     (fn [ids rep] (assoc (real-remount-outcome ids rep) :hot/down []))]
    ;; Takes any failed id as restored.
    ["failed-means-restored"
     (fn [ids rep]
       (let [up (into #{} (comp (filter :success?) (map :addon/id)) (:mounted rep))
             failed (into [] (remove up) ids)]
         (cond-> {:hot/restored failed :hot/down []}
           (seq failed) (assoc :hot/restored? true))))]
    ;; Forgets an id the report never attempted.
    ["ignores-unattempted"
     (fn [ids rep]
       (real-remount-outcome (filterv (set (map :addon/id (:mounted rep))) ids) rep))]]})

(hst/deftrifecta-from-schema remount-outcome-schema
  hive-addon.hot.report/remount-outcome
  {:in  hs/RemountOutcomeArgs
   :out hs/RemountOutcome
   :mutation false
   :num-tests 100
   :rel (fn [[ids rep] out]
          (let [up (into #{} (comp (filter :success?) (map :addon/id)) (:mounted rep))]
            (and
             ;; restored and down partition exactly the ids that are not up
             (= (set (remove up ids)) (into (set (:hot/restored out)) (:hot/down out)))
             (empty? (filter (set (:hot/restored out)) (:hot/down out)))
             ;; restored? appears iff something failed, and means nothing is down
             (= (boolean (seq (remove up ids))) (contains? out :hot/restored?))
             (or (not (contains? out :hot/restored?))
                 (= (:hot/restored? out) (empty? (:hot/down out)))))))})

;; =============================================================================
;; released-dirs
;; =============================================================================

(tri/deftrifecta released-dirs-verdict
  hive-addon.hot.report/released-dirs
  {:golden-path "test/golden/hive-addon/released-dirs.edn"
   :apply? true
   :cases {:alone   [["/x/src"] ["/a/src"]]
           :shared  [["/x/src" "/a/src"] ["/a/src"]]
           :none    [[] ["/a/src"]]
           :dupes   [["/x/src" "/x/src"] []]}
   :mutations
   [;; Releases a dir a surviving addon still lives under.
    ["ignores-remaining" (fn [ejected _] (vec (sort (distinct ejected))))]
    ;; Releases nothing — plug-out never stops watching.
    ["never-releases" (fn [_ _] [])]]})

(defspec released-dirs-never-releases-a-dir-still-in-use 200
  (prop/for-all [ejected (gen/vector (gen/elements ["/a" "/b" "/c" "/d"]))
                 remaining (gen/vector (gen/elements ["/a" "/b" "/c" "/d"]))]
    (let [out (report/released-dirs ejected remaining)]
      (and (not-any? (set remaining) out)
           (every? (set ejected) out)
           (= out (vec (sort (distinct out))))))))

(deftest a-release-with-no-claim-is-not-preservation
  (testing "the verdict is about releases, so nothing released is vacuously true"
    (is (true? (report/data-preserved? []))))
  (is (false? (report/data-preserved? [{:torn-down ["a"]}]))))
