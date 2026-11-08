(ns hive-addon.composite-facet-test
  "Facets for the pure folds of hive-addon.composite: health, tool-name
   prefixing and collision detection. hive-addon.composite-test covers the
   CompositeAddon lifecycle with plain deftests; these pin the pure decisions
   with goldens, schema properties and mutants."
  (:require [hive-addon.composite :as composite]
            [hive-schemas.test :as hst]
            [hive-test.trifecta :as tri]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(tri/deftrifecta fold-health-verdict
  hive-addon.composite/fold-health
  {:golden-path "test/golden/hive-addon/composite-fold-health.edn"
   :cases {:empty []
           :all-ok [:ok :ok]
           :all-down [:down :down]
           :mixed [:ok :down]
           :one-degraded [:ok :degraded]}
   :mutations [["ok-unless-all-down" (fn [ss] (if (and (seq ss) (every? #{:down} ss)) :down :ok))]
               ["empty-is-down" (fn [ss] (if (empty? ss) :down (composite/fold-health ss)))]]})

(hst/deftrifecta-from-schema fold-health-total
  hive-addon.composite/fold-health
  {:in [:cat [:vector [:enum :ok :degraded :down]]]
   :out [:enum :ok :degraded :down]
   ;; :mutation false - a scalar enum out leaves the schema mutants nothing to
   ;; corrupt; fold-health-verdict carries the mutants.
   :mutation false
   :num-tests 300
   :rel (fn [[ss] out]
          (cond (every? #{:ok} ss) (= :ok out)
                (every? #{:down} ss) (= :down out)
                :else (= :degraded out)))})

(tri/deftrifecta tool-prefix-names
  hive-addon.composite/tool-prefix
  {:golden-path "test/golden/hive-addon/composite-tool-prefix.edn"
   :cases {:dotted "hive.prompt.chain-of-thought"
           :slashed "acme/Search"
           :plain "carto"
           :edge-punct "x.--Weird__Name!!"}
   :mutations [["whole-id" (fn [id] (composite/tool-prefix (str/replace (str id) #"[./]" "_")))]
               ["keeps-case" (fn [id] (last (str/split (str id) #"[./]")))]]})

(tri/deftrifecta qualify-and-collide
  hive-addon.composite/collisions
  {:golden-path "test/golden/hive-addon/composite-collisions.edn"
   :cases {:none [{:member/id "a" :tool {:name "x"}} {:member/id "b" :tool {:name "y"}}]
           :clash [{:member/id "a" :tool {:name "x"}} {:member/id "b" :tool {:name "x"}}
                   {:member/id "c" :tool {:name "z"}}]}
   :mutations [["never-collides" (fn [_] {})]
               ["names-every-tool" (fn [named] (into {} (map (fn [{:keys [member/id tool]}] [(:name tool) [id]])) named))]]})
