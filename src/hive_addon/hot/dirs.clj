(ns hive-addon.hot.dirs
  "JVM adapter: IHotDirs over hive-hot's extend-init! / remove-dirs!.

   hive-hot is a SOFT dependency: both vars are resolved at CALL time, so a
   hive-hot that appears later is picked up and one that is absent (or predates
   remove-dirs!) answers an err Result naming why — never a throw. The pure
   reading of what hive-hot answered lives in hive-addon.hot.report/dirs-outcome."
  (:require [hive-addon.hot.port :as hport]
            [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- resolve-var [sym]
  (r/rescue nil (requiring-resolve sym)))

(defn- call
  "Call hive-hot's SYM with REQ: the answer as (r/ok ..), or an err Result."
  [sym req]
  (if-let [f (resolve-var sym)]
    (let [res (r/try-effect (f req))]
      (if (r/err? res)
        (r/err :hot/dirs-failed {:message (str sym " failed: " (:message res))})
        res))
    (r/err :hot/dirs-unavailable
           {:message (if (resolve-var 'hive-hot.core/reg-hot)
                       (str "hive-hot has no " (name sym)
                            " — the dirs stay watched until hive-hot is re-initialized")
                       "hive-hot is not on the classpath — nothing was watching")})))

(defrecord HiveHotDirs []
  hport/IHotDirs
  (-extend-dirs! [_ req] (call 'hive-hot.core/extend-init! req))
  (-remove-dirs! [_ req] (call 'hive-hot.core/remove-dirs! req)))

(defn hot-dirs
  "A FRESH IHotDirs bound to hive-hot.core. A function, not a def: an instance
   held across a reload of hive-addon.hot.port would stop satisfying it."
  []
  (->HiveHotDirs))
