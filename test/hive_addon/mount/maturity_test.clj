(ns hive-addon.mount.maturity-test
  "Coverage for hive-addon.mount.maturity: the :addon/maturity axis.

   The regression this file exists to prevent is the one already measured on
   the sibling axis. `:addon/trust-class` was schema-optional with a `:foss`
   default, nothing declared it in 38 manifests, and the licence gate it feeds
   was therefore never consulted by anything. Nobody noticed, because every
   manifest still VALIDATED and every read of the field returned a plausible
   answer. So the assertions here are about the two things a schema cannot say:
   which way the default leans, and whether the manifest spoke at all.

   Facet 1 synthesizes MountSpecs from the malli value objects, which draws
   `:addon/maturity` present-or-absent independently of anything else, so it
   covers the defaulting path densely. Facets 2 and 3 state the ordering
   algebra and the mutants, neither of which the schema implies."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.mount :as mount]
            [hive-addon.mount.maturity :as mat]
            [hive-addon.mount.schema :as ms]
            [hive-addon.schema :as s]
            [hive-schemas.test :as hst]
            [hive-test.mutation :as mut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; Captured before any mutation rebinds the var root, so a mutant can be phrased
;; in terms of the real fn without recursing into itself.
(def ^:private real-at-least? mat/at-least?)

(defn- spec
  ([] (spec nil))
  ([level]
   (cond-> {:addon/id "probe"
            :addon/type :native
            :addon/init-ns "probe.addon"
            :addon/init-fn "make"}
     level (assoc :addon/maturity level))))

;; =============================================================================
;; Facet 1 - schema-synthesized (hive-schemas)
;; =============================================================================
;; :mutation false - the output is a scalar keyword, so the schema-derived
;; mutant generator has no map entries to corrupt. Mutation coverage is
;; hand-authored below, where a mutant encodes the defaulting regression.

(hst/deftrifecta-from-schema maturity-defaulting
  hive-addon.mount.maturity/maturity
  {:in [:cat ms/MountSpec]
   :out s/Maturity
   :mutation false
   :num-tests 200
   :rel (fn [[sp] out]
          (and
           ;; the answer is always a declared variant, never an invention
           (contains? (:variants s/AddonMaturity) out)
           ;; a manifest that spoke is echoed verbatim
           (if (contains? sp :addon/maturity)
             (= out (:addon/maturity sp))
             ;; and one that did not gets the conservative reading, never the
             ;; strongest claim
             (and (= out mat/default-maturity)
                  (not= out :stable)))))})

;; =============================================================================
;; Facet 2 - the ordering algebra (what the schema cannot state)
;; =============================================================================

(def ^:private gen-level (gen/elements mat/levels))

(defspec at-least-is-reflexive 200
  (prop/for-all [level gen-level]
    (mat/at-least? level (spec level))))

(defspec the-weakest-floor-accepts-everything 200
  (prop/for-all [level gen-level]
    (mat/at-least? :dormant (spec level))))

(defspec at-least-is-monotone-in-the-floor 200
  (prop/for-all [strong gen-level
                 weak gen-level
                 level gen-level]
    ;; Lowering the floor can only ever admit more, never fewer.
    (let [i-strong (.indexOf ^java.util.List mat/levels strong)
          i-weak (.indexOf ^java.util.List mat/levels weak)]
      (if (and (<= i-weak i-strong) (mat/at-least? strong (spec level)))
        (mat/at-least? weak (spec level))
        true))))

(defspec at-least-is-transitive 200
  (prop/for-all [a gen-level b gen-level c gen-level]
    (if (and (mat/at-least? a (spec b)) (mat/at-least? b (spec c)))
      (mat/at-least? a (spec c))
      true)))

(deftest the-ordering-table-is-weakest-first
  (is (= [:dormant :experimental :beta :stable] mat/levels))
  (testing "each rung outranks the one below it and not the reverse"
    (doseq [[lower higher] (partition 2 1 mat/levels)]
      (is (mat/at-least? lower (spec higher))
          (str higher " should satisfy a floor of " lower))
      (is (not (mat/at-least? higher (spec lower)))
          (str lower " should NOT satisfy a floor of " higher)))))

;; =============================================================================
;; Facet 3 - the contract the trust-class lapse taught us to assert
;; =============================================================================

(deftest an-undeclared-maturity-leans-conservative
  (is (= :experimental mat/default-maturity))
  (is (= :experimental (mat/maturity (spec)))
      "an absent :addon/maturity must not read as :stable")
  (is (not (mat/at-least? :beta (spec)))
      "an undeclared addon must not pass a :beta production floor"))

(deftest declared-separates-a-deliberate-experimental-from-an-unanswered-one
  (is (not (mat/declared? (spec))))
  (is (mat/declared? (spec :experimental)))
  (testing "and the facade re-export agrees with it"
    (is (= (mat/declared? (spec)) (mount/maturity-declared? (spec)))))
  (testing "both read the same through maturity, which is why declared? exists"
    (is (= (mat/maturity (spec)) (mat/maturity (spec :experimental))))))

(deftest the-schema-refuses-a-maturity-that-is-not-a-variant
  (is (mount/validate :mount/spec (spec :beta)))
  (is (not (mount/validate :mount/spec (spec :totally-made-up))))
  (testing "the wire enum is derived from the type, so the two cannot drift"
    (is (= (set (rest s/Maturity)) (:variants s/AddonMaturity)))))

(deftest maturity-does-not-collide-with-the-store-s-addon-status
  (testing "hive-store owns :addon/status for whether a coordinate resolves,
            so a spec carrying both keys must read them independently"
    (let [sp (assoc (spec :stable) :addon/status :preview)]
      (is (= :stable (mat/maturity sp)))
      (is (= :preview (:addon/status sp)))))
  (testing "and an :addon/status alone tells maturity nothing"
    (is (= :experimental (mat/maturity (assoc (spec) :addon/status :available))))
    (is (not (mat/declared? (assoc (spec) :addon/status :available))))))

(deftest maturity-and-entitlement-are-independent-axes
  (let [proprietary-and-stable (assoc (spec :stable) :addon/trust-class :proprietary)
        foss-and-experimental (spec :experimental)]
    (is (mount/gated? proprietary-and-stable))
    (is (mat/at-least? :stable proprietary-and-stable))
    (is (not (mount/gated? foss-and-experimental)))
    (is (not (mat/at-least? :beta foss-and-experimental)))))

(deftest coerce-round-trips-through-the-type
  (is (s/addon-maturity? (mat/coerce :beta)))
  (is (= :beta (:adt/variant (mat/coerce :beta))))
  (is (nil? (mat/coerce :not-a-variant))))

;; =============================================================================
;; Mutation coverage - hand-authored, because the mutants that matter are
;; about WHICH WAY the default leans, not about corrupting a map entry.
;; =============================================================================

(mut/deftest-mutations maturity-mutations-caught
  hive-addon.mount.maturity/maturity
  [["defaults-to-stable" (fn [sp] (get sp :addon/maturity :stable))]
   ["defaults-to-beta" (fn [sp] (get sp :addon/maturity :beta))]
   ["reads-the-store-key" (fn [sp] (get sp :addon/status :experimental))]
   ["ignores-the-declaration" (fn [_] :experimental)]]
  (fn []
    (is (= :experimental (mat/maturity (spec))))
    (is (= :stable (mat/maturity (spec :stable))))
    (is (= :dormant (mat/maturity (spec :dormant))))))

(mut/deftest-mutations at-least-mutations-caught
  hive-addon.mount.maturity/at-least?
  [["always-true" (fn [_ _] true)]
   ["ignores-the-floor" (fn [_ _sp] true)]
   ["strictly-greater" (fn [floor sp] (and (real-at-least? floor sp)
                                           (not= (mat/maturity sp) floor)))]
   ["floor-and-spec-swapped" (fn [floor sp] (real-at-least? (mat/maturity sp)
                                                            (spec floor)))]]
  (fn []
    (is (true? (mat/at-least? :dormant (spec :dormant))))
    (is (false? (mat/at-least? :stable (spec :beta))))
    (is (true? (mat/at-least? :beta (spec :stable))))
    (is (false? (mat/at-least? :beta (spec))))))
