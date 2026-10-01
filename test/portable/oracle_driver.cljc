(ns portable.oracle-driver
  "The IMountDriver leg of the differential oracle — JVM and cljw only, for now.

   Split out of portable.oracle because it needs a record implementing
   hive-addon.hot.port/IMountDriver, a protocol from ANOTHER namespace, and
   cljrs cannot do that:

     $ cljrs run ... oracle_driver.cljc
     Error: reify/defrecord: hport/IMountDriver is not a protocol

   Measured 2026-08-22, cljrs 0.1.0: both `defrecord` and `reify` fail this way,
   a same-namespace protocol works, and cross-namespace `satisfies?` answers
   correctly. So the limitation is on IMPLEMENTING a foreign protocol, which is
   what a host ADAPTER does — the portable core itself is unaffected, which is
   why portable.oracle still matches on all three.

   Keeping this leg in its own file means the three-way diff of portable.oracle
   stays honest instead of being permanently red for a reason that has nothing
   to do with the stratum."
  (:require [hive-addon.hot.port :as hport]
            [hive-addon.hot.strategy :as strat]
            [portable.oracle :as oracle]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def calls (atom []))

(defrecord ProbeDriver []
  hport/IMountDriver
  (-teardown! [_ _host ids]
    (swap! calls conj [:teardown (vec ids)])
    {:torn-down (vec ids) :errors []})
  (-mount! [_ plan _host _opts]
    (swap! calls conj [:mount (mapv :addon/id (:ordered plan))])
    {:ok? true
     :mounted (mapv (fn [s] {:addon/id (:addon/id s) :success? true}) (:ordered plan))}))

(let [rep (strat/reload! {:addon/id "a"}
                         {:hot/specs oracle/specs
                          :hot/seeds #{"a"}
                          :hot/mount-driver (->ProbeDriver)
                          :hot/ns-reloaded? true})]
  (oracle/emit "remount/ok?"       (:ok? rep))
  (oracle/emit "remount/affected"  (:hot/affected rep))
  (oracle/emit "remount/torn-down" (:hot/torn-down rep))
  (oracle/emit "remount/mounted"   (mapv :addon/id (:mounted rep)))
  ;; teardown must precede mount, and both must cover the whole affected slice
  (oracle/emit "remount/calls"     @calls))

(println "ORACLE-DRIVER-END")
