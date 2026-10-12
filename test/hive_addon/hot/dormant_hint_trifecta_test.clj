(ns hive-addon.hot.dormant-hint-trifecta-test
  "Trifecta coverage for hive-addon.hot/dormant-hint: the line that keeps a
   reload of a dormant addon from passing silently as ok? true.

   Each mutant is a REAL regression: the silence this replaced (always nil),
   a hint that does not name the `hot activate` remedy, and a hint emitted
   when nothing slept."
  (:require [hive-addon.hot :as hot]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private real-dormant-hint hot/dormant-hint)

(tri/deftrifecta dormant-hint-line
  hive-addon.hot/dormant-hint
  {:golden-path "test/golden/hive-addon/dormant-hint.edn"
   :cases {:none     []
           :one      ["hive.darkmatter"]
           :two      ["probe.a" "probe.b"]
           :repeated ["probe.a" "probe.a"]}
   :mutations
   [;; The defect: a dormant reload says nothing.
    ["silent" (fn [_] nil)]
    ;; Says it slept, but not how to wake it.
    ["no-remedy"
     (fn [ids] (when (seq ids) (str (first ids) " is dormant")))]
    ;; Speaks even when nothing slept.
    ["always-speaks"
     (fn [ids] (or (real-dormant-hint ids) "nothing is dormant"))]]})
