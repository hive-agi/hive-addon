(ns hive-addon.hot.inject-trifecta-test
  "Trifecta coverage for hive-addon.hot.inject/registrable-specs: the specs an
   inject hands to hive-hot.

   Each mutant is a REAL regression: the defect it replaced (every fresh spec
   registered whether it mounted or not, duplicates kept), a duplicate id kept
   twice, and the stale (first) spec winning over the stamped fresh one."
  (:require [hive-addon.hot.inject :as inject]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- spec
  ([id] {:addon/id id :addon/init-ns (str id ".init")})
  ([id dirs] (assoc (spec id) :hot/source-dirs dirs)))

(tri/deftrifecta registrable-specs-verdict
  hive-addon.hot.inject/registrable-specs
  {:golden-path "test/golden/hive-addon/registrable-specs.edn"
   :apply? true
   :cases {:fresh-mounted     [[(spec "a")] [(spec "b" ["/b/src"])] #{"a" "b"}]
           :fresh-failed      [[(spec "a")] [(spec "slow.probe" ["/p/src"])] #{"a"}]
           :nothing-up        [[] [(spec "x")] #{}]
           :reinjected-dup    [[(spec "hive.rss") (spec "a")]
                               [(spec "hive.rss" ["/rss/src"])]
                               #{"hive.rss" "a"}]
           :dup-in-mounted    [[(spec "a") (spec "a" ["/a2"])] [] #{"a"}]
           :mixed             [[(spec "a")]
                               [(spec "b" ["/b"]) (spec "c" ["/c"])]
                               #{"a" "c"}]}
   :mutations
   [;; The defect this function replaced: hot! over ALL specs.
    ["registers-everything"
     (fn [specs fresh _up] (into (vec specs) fresh))]
    ;; Filters failed mounts but keeps duplicate ids.
    ["keeps-duplicates"
     (fn [specs fresh up]
       (into (vec specs) (filter #(contains? up (:addon/id %))) fresh))]
    ;; Dedupes, but the first (stale classpath) spec wins.
    ["first-wins"
     (fn [specs fresh up]
       (let [cands (into (vec specs) (filter #(contains? up (:addon/id %))) fresh)]
         (into [] (comp (map :addon/id) (distinct)
                        (map (fn [id] (some #(when (= id (:addon/id %)) %) cands))))
               cands)))]]})
