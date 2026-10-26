(ns hive-addon.diagnostic-facet-test
  "Facets for hive-addon.diagnostic: the guidance attached to a failed mount
   result, and the missing-addon diagnostic. Goldens over every phase that
   carries specific guidance, plus a success passthrough."
  (:require [hive-addon.diagnostic :as diagnostic]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- failed [phase] {:addon/id "probe" :success? false :phase phase})

(tri/deftrifecta mount-outcome-guidance
  hive-addon.diagnostic/mount-outcome
  {:golden-path "test/golden/hive-addon/diagnostic-mount-outcome.edn"
   :cases {:success {:addon/id "probe" :success? true :phase :initialized}
           :entitlement (failed :entitlement)
           :programs (failed :programs)
           :resolved (failed :resolved)
           :failed (failed :failed)}
   :mutations [["decorates-success" (fn [r] (assoc r :diagnostic {}))]
               ["drops-guidance" identity]]})

(tri/deftrifecta missing-addon-diagnostic
  hive-addon.diagnostic/missing-addon
  {:golden-path "test/golden/hive-addon/diagnostic-missing-addon.edn"
   :cases {:one #{"hive.rss"}
           :many ["b" "a"]}
   :mutations [["retryable" (fn [ids] (assoc (diagnostic/missing-addon ids) :retryable true))]
               ["unsorted-ids" (fn [ids] (diagnostic/missing-addon (reverse (sort ids))))]]})
