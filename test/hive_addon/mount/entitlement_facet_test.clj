(ns hive-addon.mount.entitlement-facet-test
  "Facets for the licence gate's two pure decisions: which trust classes are
   gated, and what `permit` answers for each gate shape. The fail-closed cases
   (a throwing gate, a gate that is neither protocol nor fn) are in the golden
   so a mutant that fails OPEN dies."
  (:require [hive-addon.mount.entitlement :as ent]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(tri/deftrifecta gated-by-trust-class
  hive-addon.mount.entitlement/gated?
  {:golden-path "test/golden/hive-addon/entitlement-gated.edn"
   :cases {:default-foss {}
           :native {:addon/trust-class :native}
           :proprietary {:addon/trust-class :proprietary}
           :external {:addon/trust-class :external}}
   :mutations [["never-gated" (constantly false)]
               ["always-gated" (constantly true)]]})

(def ^:private gates
  {:allow (constantly nil)
   :deny (constantly :deny/no-license)
   :throws (fn [_] (throw (ex-info "boom" {})))
   :not-a-gate 42})

(defn permit-for
  "permit under the named gate shape, so the golden is plain EDN."
  [gate-k spec]
  (ent/permit (gates gate-k) spec))

(def ^:private prop {:addon/id "p" :addon/trust-class :proprietary})

(tri/deftrifecta permit-shapes
  hive-addon.mount.entitlement-facet-test/permit-for
  {:golden-path "test/golden/hive-addon/entitlement-permit.edn"
   :apply? true
   :cases {:ungated-any-gate [:deny {:addon/id "f"}]
           :allow [:allow prop]
           :deny [:deny prop]
           :throws-fails-closed [:throws prop]
           :non-gate-fails-closed [:not-a-gate prop]}
   :mutations [["fails-open-on-throw"
                (fn [g s] (if (= :throws g) nil (permit-for g s)))]
               ["ignores-trust-class"
                (fn [g s] (ent/permit (gates g) (assoc s :addon/trust-class :proprietary)))]]})
