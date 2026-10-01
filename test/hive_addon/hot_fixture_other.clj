(ns hive-addon.hot-fixture-other
  "A SECOND constructor namespace for the hot-reload suite.

   The whole suite otherwise mounts every probe addon from
   `hive-addon.hot-fixture`, so no test could tell an addon inside the reloaded
   slice from one outside it — the two were the same namespace. This one exists
   to make that distinction expressible: an addon built from HERE is reached
   only when the reloader reports THIS namespace.

   Generation counting stays in the primary fixture, so a test compares the two
   namespaces' addons on one counter."
  (:require [hive-addon.hot-fixture :as fx]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn make-other
  "Build a probe addon named by the config's :probe/id."
  [config]
  (fx/make-shared config))
