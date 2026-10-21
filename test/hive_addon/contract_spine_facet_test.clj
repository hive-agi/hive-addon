(ns hive-addon.contract-spine-facet-test
  "Facets for the newly contracted public API of the IAddon contract layer:
   hive-addon.schema (registry wrappers + Result bridge + validate-addon),
   hive-addon.capability (declared-ness derivations) and hive-addon.extension
   (the extension-point report).

   Two kinds of evidence. The trifectas pin behaviour with goldens and catch
   real regressions as mutants. `contracts-hold-under-instrumentation` runs a
   battery through the m/=> contracts themselves, instrumented and restored
   per test, so a contract that misstates its function fails here rather than
   in a host that turns instrumentation on."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.capability :as cap]
            [hive-addon.extension :as ext]
            [hive-addon.schema :as s]
            [hive-schemas.test :as hst]
            [hive-test.trifecta :as tri]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private real-report ext/report)

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- point [cap & {:as extra}]
  (merge {:extension/capability cap
          :extension/summary "A seam the host accepts providers for."}
         extra))

(def ^:private host
  {:addon/id "host" :addon/capabilities #{:tools}
   :addon/extension-points [(point :sources :extension/cardinality :cardinality/one)
                            (point :sinks)]})

(def ^:private rival-a {:addon/id "a" :addon/capabilities #{:sources}})
(def ^:private rival-b {:addon/id "b" :addon/capabilities #{:sources :orphan}})

(def ^:private manifest
  {:tool "analysis" :owner :hive/analysis :description "Static analysis."
   :all-commands #{"lint" "help" "kondo"}
   :commands [{:command "lint" :summary "Lint one file." :schema :analysis/lint}
              {:command "kondo" :summary "Run clj-kondo over a path."}]})

;; =============================================================================
;; capability/status: declared-ness is exactly the presence of :schema
;; =============================================================================

(tri/deftrifecta capability-status
  hive-addon.capability/status
  {:golden-path "test/golden/hive-addon/capability-status.edn"
   :cases {:schematized {:command "x" :summary "summary!" :schema :a/b}
           :summary-only {:command "x" :summary "summary!"}
           :empty {}}
   :mutations [["always-declared" (fn [_] :capability/declared)]
               ["inverted" (fn [{:keys [schema]}]
                             (if schema :capability/undeclared :capability/declared))]]})

(hst/deftrifecta-from-schema capability-status-schema
  hive-addon.capability/status
  {:in  [:map [:schema {:optional true} [:maybe :qualified-keyword]]]
   :out [:enum :capability/declared :capability/undeclared]
   :mutation false
   :num-tests 100
   :rel (fn [in out] (= (some? (:schema in)) (= :capability/declared out)))})

(deftest capability-sets-partition-the-dispatch-surface
  (is (= #{"lint" "kondo"} (cap/declared manifest)))
  (is (= #{"lint"} (cap/schematized manifest)))
  (is (= #{"help"} (cap/undeclared manifest)))
  (is (= (:all-commands manifest)
         (set/union (cap/declared manifest) (cap/undeclared manifest))))
  (is (= #{"lint" "kondo"} (set (keys (cap/index manifest))))))

;; =============================================================================
;; extension/report: the five derived diagnostics
;; =============================================================================

(tri/deftrifecta extension-report
  hive-addon.extension/report
  {:golden-path "test/golden/hive-addon/extension-report.edn"
   :cases {:none []
           :vacant [host]
           :rivals [host rival-a rival-b]
           :single-tenant [host rival-a]
           ;; :sinks is :cardinality/many: two tenants are legal, not a clash.
           :many-rivals [host {:addon/id "c" :addon/capabilities #{:sinks}}
                         {:addon/id "d" :addon/capabilities #{:sinks}}]}
   :mutations [;; Ignores cardinality: every multi-tenant point is a clash.
               ["many-is-clash"
                (fn [specs]
                  (assoc (real-report specs) :over-subscribed
                         (into {} (keep (fn [sp]
                                          (let [c (into {} (keep (fn [p]
                                                                   (let [ts (ext/tenants specs (:extension/capability p))]
                                                                     (when (> (count ts) 1) [(:extension/capability p) ts]))))
                                                        (ext/points-of sp))]
                                            (when (seq c) [(:addon/id sp) c]))))
                               specs)))]
               ;; Never reports a stray offer.
               ["no-unconsumed" (fn [specs] (assoc (real-report specs) :unconsumed {}))]
               ;; Counts the opener as its own tenant, so nothing is vacant.
               ["opener-fills" (fn [specs] (assoc (real-report specs) :vacant {}))]]})

(def ^:private gen-ecosystem
  "A host opening :p (cardinality one or many) plus 0-4 providers drawn over
   {:p :q}. Constructed, not synthesized, so tenancy is actually exercised."
  (gen/let [card (gen/elements [:cardinality/one :cardinality/many])
            caps (gen/vector (gen/set (gen/elements [:p :q]) {:max-elements 2}) 0 4)]
    (into [{:addon/id "host" :addon/extension-points [(point :p :extension/cardinality card)]}]
          (map-indexed (fn [i cs] {:addon/id (str "t" i) :addon/capabilities cs}))
          caps)))

(defspec report-diagnostics-are-consistent 200
  (prop/for-all [specs gen-ecosystem]
    (let [{:keys [providers vacant over-subscribed]} (ext/report specs)
          tenants (get providers :p #{})
          one? (= :cardinality/one (-> specs first :addon/extension-points first :extension/cardinality))]
      (and (= (empty? tenants) (contains? vacant "host"))
           (= (and one? (> (count tenants) 1)) (contains? over-subscribed "host"))
           (or (not (contains? over-subscribed "host"))
               (= tenants (get-in over-subscribed ["host" :p])))))))

;; =============================================================================
;; The contracts themselves, instrumented
;; =============================================================================

(defn- with-instrumented
  "Instrument every m/=> registered under `nss`, run `f`, restore the roots."
  [nss f]
  (let [originals (atom {})]
    (try
      (doseq [[n fns] (select-keys (m/function-schemas) nss)
              [fname {:keys [schema]}] fns
              :let [v (find-var (symbol (str n) (str fname)))]]
        (swap! originals assoc v @v)
        (alter-var-root v (fn [orig]
                            (m/-instrument
                             {:schema schema
                              :report (fn [t d]
                                        (throw (ex-info (str t " " n "/" fname)
                                                        (select-keys d [:args :value]))))}
                             orig))))
      (f (count @originals))
      (finally
        (doseq [[v o] @originals] (alter-var-root v (constantly o)))))))

(deftest contracts-hold-under-instrumentation
  (with-instrumented
    '[hive-addon.schema hive-addon.capability hive-addon.extension]
    (fn [n]
      (is (<= 28 n) "the three namespaces carry their contracts")
      (testing "hive-addon.schema"
        (is (m/schema? (s/schema [:map [:a :int]])))
        (is (true? (s/validate :addon/id "x")))
        (is (false? (s/validate string? 1)))
        (is (nil? (s/explain :int 1)))
        (is (= ["should be an integer"] (s/humanize-errors :int "x")))
        (is (= {:ok 1} (s/validate* :int 1)))
        (is (= :a/b (:error (s/validate* :int "x" :a/b))))
        (is (= :addon/contract-violation (:error (s/validate-addon nil)))))
      (testing "hive-addon.capability"
        (is (= :capability/undeclared (cap/status {})))
        (is (= #{"help"} (cap/undeclared manifest)))
        (is (= #{"lint"} (cap/schematized manifest))))
      (testing "hive-addon.extension"
        (let [r (ext/report [host rival-a rival-b])]
          (is (= {"host" {:sources #{"a" "b"}}} (:over-subscribed r)))
          (is (= {"host" #{:sinks}} (:vacant r)))
          (is (= {"b" #{:orphan}} (:unconsumed r))))
        (is (= #{:sources :sinks} (set (keys (ext/index host)))))
        (is (= :extension/schema-violation (:error (ext/validate* :extension/point {}))))
        (is (some? (ext/explain :extension/point {})))
        (is (true? (ext/validate :extension/point (point :x))))))))

(deftest a-misstated-contract-is-caught
  ;; Non-vacuity: instrumentation must reject a call outside the domain.
  (with-instrumented
    '[hive-addon.capability]
    (fn [_]
      (is (thrown? clojure.lang.ExceptionInfo
                   (cap/declared {:tool "t"}))))))
