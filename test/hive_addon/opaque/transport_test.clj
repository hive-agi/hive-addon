(ns hive-addon.opaque.transport-test
  "Facets for the ITransport port and its two adapters, driven through the
   port's own methods: the in-process transport's lifecycle (nil before start
   and after stop, like a dead pipe) and `down-reason` over a transport with
   and without the optional IDiagnosable companion."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.opaque.transport :as t]
            [hive-addon.opaque.transport.inproc :as inproc]
            [hive-addon.opaque.transport.subprocess :as sub]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn lifecycle-script
  "What an fn-transport answers at each lifecycle step, as data."
  [line]
  (let [tr (inproc/fn-transport (fn [l] (str "echo:" l)))
        before (t/-request! tr line)
        _ (t/-start! tr)
        alive (t/-alive? tr)
        during (t/-request! tr line)
        _ (t/-stop! tr)]
    {:before before :alive alive :during during
     :after (t/-request! tr line) :alive-after (t/-alive? tr)
     :down-reason (t/down-reason tr)}))

(tri/deftrifecta inproc-lifecycle
  hive-addon.opaque.transport-test/lifecycle-script
  {:golden-path "test/golden/hive-addon/opaque-transport-lifecycle.edn"
   :cases {:plain "ping" :empty ""}
   :mutations [["answers-before-start"
                (fn [l] (assoc (lifecycle-script l) :before (str "echo:" l)))]
               ["survives-stop"
                (fn [l] (assoc (lifecycle-script l) :after (str "echo:" l) :alive-after true))]]})

(deftest subprocess-transport-never-started-has-no-reason
  ;; A transport that was never started, or was stopped on request, reports no
  ;; reason: the reason is for a transport that DIED.
  (let [tr (sub/subprocess-transport ["/bin/true"])]
    (is (false? (t/-alive? tr)))
    (is (nil? (t/down-reason tr)))
    (is (nil? (t/-stop! tr)) "stop is idempotent on a never-started transport")))

(deftest down-reason-is-nil-for-a-plain-itransport
  (is (nil? (t/down-reason (reify t/ITransport
                             (-start! [this] this)
                             (-request! [_ _] nil)
                             (-alive? [_] false)
                             (-stop! [_] nil))))))
