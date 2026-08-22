(ns hive-addon.hot.mount-driver
  "JVM adapter: IMountDriver over hive-addon.mount.boundary.

   boundary reaches the classpath, JarFile, URL and the context classloader, so
   it is the host-bound half of a remount and lives behind the port rather than
   inside the portable core."
  (:require [hive-addon.hot.port :as hport]
            [hive-addon.mount.boundary :as boundary]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defrecord BoundaryMountDriver []
  hport/IMountDriver
  (-teardown! [_ host ids]
    (boundary/teardown! host ids))
  (-mount! [_ plan host opts]
    (boundary/mount! plan host opts)))

(defn mount-driver
  "A FRESH IMountDriver bound to hive-addon.mount.boundary.

   A function, not a def: an instance held across a reload of hive-addon.hot.port
   would carry the previous protocol's class object and stop satisfying it."
  []
  (->BoundaryMountDriver))
