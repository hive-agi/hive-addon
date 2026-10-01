(ns hive-addon.lifecycle.oracle
  "Which addons are dormant right now, for code that must not act on them.

   A reload cascade consults this so a file change under a dormant addon does not
   mount it behind the lifecycle's back. The installed predicate is read at call
   time; the default answers false for every id, which is the behaviour of a host
   with no lifecycle.

   Portable stratum.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defonce ^:private dormant-fn (atom nil))

(defn install-dormancy!
  "Install F, (fn [addon-id] -> truthy when dormant). Returns F."
  [f]
  (reset! dormant-fn f)
  f)

(defn reset-dormancy!
  "Back to 'nothing is dormant'."
  []
  (reset! dormant-fn nil)
  nil)

(defn dormant?
  "True when the installed predicate says ADDON-ID is dormant."
  [addon-id]
  (if-let [f @dormant-fn]
    (boolean (f addon-id))
    false))
