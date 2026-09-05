(ns hive-addon.extension-test
  "Coverage for hive-addon.extension: the third capability edge and the
   derivations over it.

   TWO generators, per the joint-distribution rule the solve trifecta
   established. hive-schemas synthesizes MountSpecs from the malli value
   objects, but `:addon/extension-points` is OPTIONAL and its capability is
   drawn independently of any provider's `:addon/capabilities`, so on its own
   every clause about tenancy would pass VACUOUSLY: the synthesized universe is
   almost entirely specs that open nothing and fill nothing. `gen-ecosystem`
   CONSTRUCTS the host/provider relation instead, and
   `structured-generator-actually-relates` is the guard that keeps it honest."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.extension :as ext]
            [hive-addon.protocol :as proto]
            [hive-addon.mount.schema :as ms]
            [hive-addon.mount.compose :as compose]
            [hive-dsl.result :as r]
            [hive-schemas.test :as hst]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- spec
  "A minimal valid MountSpec carrying `extra`."
  [id extra]
  (merge {:addon/id id
          :addon/type :native
          :addon/init-ns (str "probe." id)
          :addon/init-fn "make"}
         extra))

(defn- point
  ([cap] (point cap {}))
  ([cap extra]
   (merge {:extension/capability cap
           :extension/summary "A corpus the host can pull documents from."}
          extra)))

(def ^:private host
  (spec "hive.ingestor"
        {:addon/capabilities #{:tools}
         :addon/extension-points [(point :sources {:extension/port "hive-ingestor.source/ISource"
                                                   :extension/registry "hive-ingestor.source.registry"
                                                   :extension/schema :ingestor/source-params})]}))

(def ^:private tenant
  (spec "hive.ingestor.rfc" {:addon/capabilities #{:sources}}))

(def ^:private stray
  (spec "hive.stray" {:addon/capabilities #{:nobody-accepts-this}}))

;; =============================================================================
;; Value objects
;; =============================================================================

(deftest a-point-needs-a-capability-and-a-summary
  (is (ext/valid-point? (point :sources)))
  (testing "the summary is bounded, so a card has something to render and not an essay"
    (is (not (ext/valid-point? (assoc (point :sources) :extension/summary "short"))))
    (is (not (ext/valid-point? (assoc (point :sources)
                                      :extension/summary (apply str (repeat 201 "x")))))))
  (testing "a port names a protocol as a STRING, because a manifest resolves no symbol"
    (is (ext/valid-point? (point :sources {:extension/port "hive-ingestor.source/ISource"})))
    (is (not (ext/valid-point? (point :sources {:extension/port 'hive-ingestor.source/ISource}))))))

(deftest cardinality-is-a-closed-set
  (is (= #{:cardinality/one :cardinality/many} ext/cardinalities))
  (is (ext/valid-point? (point :sources {:extension/cardinality :cardinality/one})))
  (is (not (ext/valid-point? (point :sources {:extension/cardinality :cardinality/some})))))

(deftest stability-is-the-capability-vocabulary-not-a-second-copy
  (is (ext/valid-point? (point :sources {:extension/stability :stability/experimental})))
  (is (not (ext/valid-point? (point :sources {:extension/stability :stability/probably})))))

(deftest one-addon-opens-a-capability-at-most-once
  (is (ext/validate ext/ExtensionPoints [(point :sources) (point :detectors)]))
  (is (not (ext/validate ext/ExtensionPoints [(point :sources) (point :sources)]))))

(deftest a-mount-spec-carries-its-points
  (is (ms/validate ms/MountSpec host))
  (testing "and a spec declaring none is still valid, so the field is additive"
    (is (ms/validate ms/MountSpec tenant)))
  (testing "a duplicated capability fails at the manifest boundary"
    (is (not (ms/validate ms/MountSpec
                          (assoc host :addon/extension-points [(point :sources) (point :sources)])))))
  (testing "validate* reports the violation as a Result rather than throwing"
    (let [res (ext/validate* ext/ExtensionPoints [(point :sources) (point :sources)])]
      (is (r/err? res))
      (is (= :extension/schema-violation (:error res))))))

;; =============================================================================
;; Derivations
;; =============================================================================

(deftest openers-and-providers-are-the-two-sides-of-one-join
  (is (= {:sources #{"hive.ingestor"}} (ext/openers [host tenant])))
  (is (= {:tools #{"hive.ingestor"} :sources #{"hive.ingestor.rfc"}}
         (ext/providers [host tenant]))))

(deftest a-tenant-is-a-provider-that-is-not-the-opener
  (is (= #{"hive.ingestor.rfc"} (ext/tenants [host tenant] :sources)))
  (testing "a host that fills its own seam has not proven the seam"
    (let [self (assoc host :addon/capabilities #{:tools :sources})]
      (is (empty? (ext/tenants [self] :sources)))
      (is (= {"hive.ingestor" #{:sources}} (ext/vacant [self]))))))

(deftest vacant-names-a-seam-no-provider-has-exercised
  (is (= {"hive.ingestor" #{:sources}} (ext/vacant [host])))
  (is (empty? (ext/vacant [host tenant]))))

(deftest unconsumed-names-a-registration-nothing-can-reach
  (is (= {"hive.stray" #{:nobody-accepts-this}} (ext/unconsumed [host tenant stray])))
  (testing "a standard capability is always accounted for"
    (is (empty? (ext/unconsumed [host]))))
  (testing "so is one some addon requires of its host, even with no point for it"
    (let [needs (spec "hive.flow" {:addon/requires-capabilities #{:vessel}})
          offers (spec "hive.emacs" {:addon/capabilities #{:vessel}})]
      (is (empty? (ext/unconsumed [needs offers]))))))

(deftest over-subscribed-catches-rivals-on-a-cardinality-one-point
  (let [sole (assoc host :addon/extension-points
                    [(point :sources {:extension/cardinality :cardinality/one})])
        other (spec "hive.vtranslate" {:addon/capabilities #{:sources}})]
    (is (empty? (ext/over-subscribed [sole tenant])))
    (is (= {"hive.ingestor" {:sources #{"hive.ingestor.rfc" "hive.vtranslate"}}}
           (ext/over-subscribed [sole tenant other])))
    (testing "a :cardinality/many point never contends"
      (is (empty? (ext/over-subscribed [host tenant other]))))))

(deftest report-is-derived-and-stores-nothing
  (let [rep (ext/report [host tenant stray])]
    (is (= #{:openers :providers :vacant :unconsumed :over-subscribed} (set (keys rep))))
    (is (= (ext/openers [host tenant stray]) (:openers rep)))
    (is (empty? (:vacant rep)))
    (is (= {"hive.stray" #{:nobody-accepts-this}} (:unconsumed rep)))))

;; =============================================================================
;; The outermost caller
;; =============================================================================
;; A seam with zero tenants has never been exercised end to end, and the way
;; that hides is that every assertion drives the registry API directly. These
;; go through compose-plan, which is what a host actually calls.

(deftest compose-plan-reports-the-extension-graph
  (testing "with no plug layers, every discovered spec is surveyed"
    (let [res (compose/compose-plan [host tenant] [])]
      (is (r/ok? res))
      (is (= {:sources #{"hive.ingestor"}} (get-in res [:ok :extensions :openers])))
      (is (empty? (get-in res [:ok :extensions :vacant])))))
  (testing "and a host mounted without its tenant is reported vacant"
    (let [res (compose/compose-plan [host] [])]
      (is (r/ok? res))
      (is (= {"hive.ingestor" #{:sources}} (get-in res [:ok :extensions :vacant])))))
  (testing "a spec registering into nothing is named, not silently mounted"
    (let [res (compose/compose-plan [host tenant stray] [])]
      (is (= {"hive.stray" #{:nobody-accepts-this}}
             (get-in res [:ok :extensions :unconsumed]))))))

;; =============================================================================
;; Facet 1: schema-synthesized (hive-schemas)
;; =============================================================================
;; :mutation false for the same reason the solve trifecta gives: a mutant
;; derived from the required keys of a map-shaped :out perturbs diagnostic keys
;; and says nothing about the JOIN. The relation clauses below are structural
;; and hold on the synthesized universe as well as the constructed one.

(def ^:private ReportArgs
  "Arglist of hive-addon.extension/report. A :cat schema, so a schema-driven
   test APPLIES the subject rather than handing it the vector as one argument."
  [:cat [:sequential ms/MountSpec]])

(def ^:private ReportOut
  [:map {:closed true}
   [:openers         [:map-of :keyword [:set :string]]]
   [:providers       [:map-of :keyword [:set :string]]]
   [:vacant          [:map-of :string [:set :keyword]]]
   [:unconsumed      [:map-of :string [:set :keyword]]]
   [:over-subscribed [:map-of :string [:map-of :keyword [:set :string]]]]])

(defn- mirrors?
  "No capability is BOTH vacant somewhere and unconsumed somewhere.

   Vacant means a point nobody fills; unconsumed means an offer nothing accepts.
   A capability that is opened is accounted for by definition, so it can never
   be unconsumed, and one that is provided has a tenant unless its only provider
   is the opener itself. The two diagnostics partition the failure, and an
   overlap would mean the same capability was read two different ways."
  [rep]
  (empty? (set/intersection
           (into #{} cat (vals (:vacant rep)))
           (into #{} cat (vals (:unconsumed rep))))))

(defn- unconsumed-is-never-opened-or-standard? [specs rep]
  (let [opened (set (keys (ext/openers specs)))
        needed (into #{} (mapcat :addon/requires-capabilities) specs)]
    (every? (fn [c] (and (not (opened c))
                         (not (needed c))
                         (not (contains? proto/standard-capabilities c))))
            (into #{} cat (vals (:unconsumed rep))))))

(hst/deftrifecta-from-schema report-schema
  hive-addon.extension/report
  {:in  ReportArgs
   :out ReportOut
   :mutation false
   :num-tests 100
   :rel (fn [[specs] rep]
          (let [ids (into #{} (map :addon/id) specs)]
            (and
             ;; nothing is invented: every reported id was an input spec
             (every? ids (keys (:vacant rep)))
             (every? ids (keys (:unconsumed rep)))
             (every? ids (into #{} cat (vals (:providers rep))))
             (every? ids (into #{} cat (vals (:openers rep))))
             ;; only an addon that opened a point can be vacant
             (every? (fn [[id caps]]
                       (let [s (first (filter #(= id (:addon/id %)) specs))]
                         (every? (ext/point-capabilities s) caps)))
                     (:vacant rep))
             (mirrors? rep)
             (unconsumed-is-never-opened-or-standard? specs rep))))})

;; =============================================================================
;; Facet 2: structured ecosystem generator (constructs the relation)
;; =============================================================================

(def ^:private caps [:sources :detectors :vessel :lenses :backends])

(def gen-ecosystem
  "A spec set whose openers and providers REFERENCE the same capability pool.

   The pool is small and shared on purpose. Drawing a host's point capability
   and a provider's offered capability from the same five keywords is what makes
   a tenancy actually occur; independent draws over the keyword universe produce
   a join that is empty on essentially every sample, which is the vacuous pass
   this facet exists to avoid.

   Ids are positional (n0..n(n-1)) rather than drawn, so no sample carries the
   same :addon/id twice. A duplicate id would let two specs collide in the
   id-keyed diagnostics and fail a clause for a reason that is not the subject."
  (gen/let [n (gen/choose 1 6)]
    (apply gen/tuple
           (map (fn [i]
                  (gen/let [opens  (gen/set (gen/elements caps) {:max-elements 2})
                            offers (gen/set (gen/elements caps) {:max-elements 2})
                            std    (gen/set (gen/elements (vec proto/standard-capabilities))
                                            {:max-elements 2})]
                    (spec (str "n" i)
                          {:addon/capabilities (into offers std)
                           :addon/extension-points (mapv point opens)})))
                (range n)))))

(deftest structured-generator-actually-relates
  ;; The guard, and the first thing to look at if the properties below ever go
  ;; green while the join is broken. Every clause about tenancy is vacuously
  ;; true on a sample where nobody fills anything, and vacancy is the COMMON
  ;; outcome, so a per-sample property cannot assert this. It has to be said
  ;; across the sample set.
  (let [samples (gen/sample gen-ecosystem 200)
        tenanted (filter (fn [specs]
                           (let [rep (ext/report specs)]
                             (some (fn [c] (seq (ext/tenants specs c)))
                                   (keys (:openers rep)))))
                         samples)
        vacant-somewhere (filter #(seq (ext/vacant %)) samples)]
    (testing "some samples fill a point, or every tenancy clause passes for an empty reason"
      (is (seq tenanted)))
    (testing "and some leave one empty, or the vacancy clauses do the same"
      (is (seq vacant-somewhere)))))

(defspec structured-vacant-and-unconsumed-never-overlap 200
  (prop/for-all [specs gen-ecosystem]
    (mirrors? (ext/report specs))))

(defspec structured-a-tenant-is-never-its-own-opener 200
  (prop/for-all [specs gen-ecosystem]
    (let [rep (ext/report specs)]
      (every? (fn [[c openers]]
                (empty? (set/intersection openers (ext/tenants specs c))))
              (:openers rep)))))

(defspec structured-vacant-is-exactly-a-point-with-no-tenant 200
  (prop/for-all [specs gen-ecosystem]
    (let [rep (ext/report specs)]
      (every? (fn [s]
                (let [reported (get (:vacant rep) (:addon/id s) #{})]
                  (= reported
                     (into #{} (remove #(seq (ext/tenants specs %)))
                           (ext/point-capabilities s)))))
              specs))))
