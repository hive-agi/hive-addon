(ns hive-addon.opaque.addon-test
  "Facets for the host-side proxy's one call across the wire: `call!` turns
   every failure into an error response, and `call-result!` answers the
   fallback for each. The transports are line functions (the ITransport port
   satisfied by hive-addon.opaque.transport.inproc), so each failure is the
   real bytes a broken kernel would send."
  (:require [hive-addon.opaque :as opaque]
            [hive-addon.opaque.addon :as oaddon]
            [hive-addon.opaque.opaque-facets :as fx]
            [hive-addon.opaque.serve :as serve]
            [hive-addon.opaque.transport :as t]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private line-fns
  {:live (fn [l] (serve/handle-line (fx/kernel) l))
   :closed (constantly nil)
   :garbage (constantly "[not a map")
   :vector (constantly "[1 2]")
   :throws (fn [_] (throw (ex-info "deadline missed" {})))})

(defn call-over
  "call! for OP over the named transport, started."
  [transport-k op args]
  (let [tr (t/-start! (opaque/line-transport (line-fns transport-k)))]
    (oaddon/call! tr op args)))

(tri/deftrifecta call-never-throws
  hive-addon.opaque.addon-test/call-over
  {:golden-path "test/golden/hive-addon/opaque-addon-call.edn"
   :apply? true
   :cases {:health [:live :addon/health nil]
           :tool [:live :addon/tool {:tool "double" :params {:x 4}}]
           :closed-pipe [:closed :addon/health nil]
           :unreadable [:garbage :addon/health nil]
           :not-a-map [:vector :addon/health nil]
           :transport-throws [:throws :addon/tool {:tool "double"}]}
   :mutations [["nil-on-eof" (fn [tk op args] (if (= :closed tk) nil (call-over tk op args)))]
               ["loses-cause" (fn [tk op args]
                                (let [a (call-over tk op args)]
                                  (if (:error a) (assoc a :error "error") a)))]]})

(defn result-over
  [transport-k]
  (oaddon/call-result! (t/-start! (opaque/line-transport (line-fns transport-k)))
                       :addon/health nil ::fallback))

(tri/deftrifecta call-result-falls-back
  hive-addon.opaque.addon-test/result-over
  {:golden-path "test/golden/hive-addon/opaque-addon-call-result.edn"
   :cases {:live :live :closed :closed :throws :throws}
   :mutations [["never-falls-back" (fn [tk] (when (= :live tk) (result-over tk)))]
               ["always-falls-back" (constantly ::fallback)]]})
