(ns hive-addon.tool-contract-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.generator :as mg]
            [hive-addon.protocol :as proto]
            [hive-addon.tool-contract :as contract]
            [hive-addon.tool-contract.check :as check]
            [hive-addon.tool-contract.test :as ct]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def good
  {:name        "frob"
   :description "Frobnicate a widget."
   :inputSchema {:type       "object"
                 :properties {"widget" {:type "string" :description "Widget id"}}
                 :required   ["widget"]}
   :handler     identity})

(defn- paths [tool] (set (map :path (contract/violations tool))))

;; ── golden ──────────────────────────────────────────────────────────────────

(deftest a-described-object-schema-conforms
  (is (contract/conforms? good))
  (is (= [] (contract/violations good)))
  (is (= [] (contract/warnings good)))
  (is (identical? good (contract/assert-root-tool! good))))

(deftest keyword-type-and-keyword-property-names-conform
  (is (contract/conforms? (assoc good :inputSchema {:type :object
                                                    :properties {:widget {:type "string"}}}))))

;; ── each violation, one mutation at a time ──────────────────────────────────

(deftest empty-properties-is-a-violation
  (is (= #{[:inputSchema :properties]}
         (paths (assoc-in good [:inputSchema :properties] {})))))

(deftest missing-schema-is-a-violation
  (is (= #{[:inputSchema]} (paths (dissoc good :inputSchema))))
  (is (= #{[:inputSchema]} (paths (assoc good :inputSchema nil)))))

(deftest schema-without-properties-is-a-violation
  (is (= #{[:inputSchema :properties]}
         (paths (assoc good :inputSchema {:type "object"})))))

(deftest non-object-type-is-a-violation
  (is (= #{[:inputSchema :type]}
         (paths (assoc-in good [:inputSchema :type] "string"))))
  (is (= #{[:inputSchema :type]}
         (paths (update good :inputSchema dissoc :type)))))

(deftest blank-name-or-description-is-a-violation
  (is (= #{[:description]} (paths (assoc good :description "  "))))
  (is (= #{[:description]} (paths (dissoc good :description))))
  (is (= #{[:name]} (paths (assoc good :name "")))))

(deftest a-non-map-property-schema-is-a-violation
  (is (seq (contract/violations (assoc-in good [:inputSchema :properties] {"x" "string"})))))

(deftest undescribed-property-is-a-warning-not-a-violation
  (let [t (assoc-in good [:inputSchema :properties "widget"] {:type "string"})]
    (is (contract/conforms? t))
    (is (= [[:inputSchema :properties "widget" :description]]
           (mapv :path (contract/warnings t))))))

;; ── generative ──────────────────────────────────────────────────────────────

(defspec every-generated-root-tool-conforms 100
  (prop/for-all [t (mg/generator contract/RootToolDef)]
    (contract/conforms? t)))

(defspec emptying-properties-always-violates 100
  (prop/for-all [t (mg/generator contract/RootToolDef)]
    (not (contract/conforms? (assoc-in t [:inputSchema :properties] {})))))

(defspec violations-and-conforms-agree 200
  (prop/for-all [t (gen/one-of [(mg/generator contract/RootToolDef)
                                (gen/map gen/keyword gen/any-printable-equatable)])]
    (= (contract/conforms? t) (empty? (contract/violations t)))))

;; ── boundary ────────────────────────────────────────────────────────────────

(deftest assert-root-tools-names-every-offender-in-one-throw
  (let [bad-a (assoc good :name "a" :inputSchema {:type "object" :properties {}})
        bad-b (assoc good :name "b" :description "")
        e     (try (contract/assert-root-tools! [good bad-a bad-b]) nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= :hive-addon/root-tool-contract (:type (ex-data e))))
    (is (= #{"a" "b"} (set (keys (:violations (ex-data e))))))
    (is (re-find #"a \[:inputSchema :properties\]" (ex-message e)))))

(deftest assert-root-tools-passes-a-conforming-seq-through
  (is (= [good] (contract/assert-root-tools! [good]))))

;; ── compile time ────────────────────────────────────────────────────────────

(defn- compile-error [form]
  (try (binding [*ns* (the-ns 'hive-addon.tool-contract-test)] (eval form)) nil
       (catch Throwable e (or (ex-data (ex-cause e)) (ex-data e) e))))

(deftest def-root-tool-refuses-an-empty-literal-schema-at-macroexpansion
  (let [data (compile-error
              '(hive-addon.tool-contract/def-root-tool bad-tool
                 {:name "bad" :description "d" :handler identity
                  :inputSchema {:type "object" :properties {}}}))]
    (is (= :hive-addon/root-tool-contract (:type data)))
    (is (not (resolve 'hive-addon.tool-contract-test/bad-tool))
        "the def never happened")))

(deftest def-root-tool-refuses-a-literal-without-schema
  (is (= :hive-addon/root-tool-contract
         (:type (compile-error
                 '(hive-addon.tool-contract/def-root-tool bad-tool-2
                    {:name "bad" :description "d" :handler identity}))))))

(def ^:private dynamic-props {})

(deftest def-root-tool-checks-a-computed-schema-when-the-def-runs
  (is (= :hive-addon/root-tool-contract
         (:type (compile-error
                 '(hive-addon.tool-contract/def-root-tool bad-tool-3
                    {:name "bad" :description "d" :handler identity
                     :inputSchema {:type "object" :properties dynamic-props}}))))))

(contract/def-root-tool defined-good-tool good)

(deftest def-root-tool-defs-a-conforming-tool
  (is (= good defined-good-tool)))

(deftest expansion-view-only-substitutes-non-literal-contract-values
  (is (nil? (contract/expansion-view 'some-symbol)))
  (is (= {} (get-in (contract/expansion-view {:inputSchema {:type "object" :properties {}}})
                    [:inputSchema :properties])))
  (is (contract/conforms? (contract/expansion-view
                           '{:name n :description (str "x") :inputSchema (schema)}))))

;; ── CI helpers ──────────────────────────────────────────────────────────────

(defn- captured-reports [f]
  (let [out (atom [])]
    (binding [t/report (fn [m] (swap! out conj (:type m)))]
      (f))
    @out))

(deftest assert-root-tools-reports-one-failure-per-offender
  (let [types (captured-reports
               #(ct/assert-root-tools [good (assoc-in good [:inputSchema :properties] {})]))]
    (is (= 1 (count (filter #{:fail} types))))))

(deftest assert-root-tools-fails-an-empty-seq-unless-allowed
  (is (= [:fail] (captured-reports #(ct/assert-root-tools []))))
  (is (= [:pass] (captured-reports #(ct/assert-root-tools [] {:allow-empty? true})))))

(ct/deftest-root-tools the-macro-defines-a-passing-deftest [good])

(defrecord StubAddon [tool-defs]
  proto/IAddon
  (addon-id [_] "stub")
  (addon-type [_] :native)
  (capabilities [_] #{:tools})
  (initialize! [_ _] {:success? true})
  (shutdown! [_] nil)
  (tools [_] tool-defs)
  (schema-extensions [_] [])
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] {}))

(defn ->good-addon [] (->StubAddon [good]))
(defn ->bad-addon [] (->StubAddon [(assoc-in good [:inputSchema :properties] {})]))

(deftest check-verdict-exits-by-severity
  (is (= 0 (:exit (check/verdict [(check/addon-tools `->good-addon)]))))
  (is (= 1 (:exit (check/verdict [(check/addon-tools `->good-addon)
                                  (check/addon-tools `->bad-addon)]))))
  (is (= 2 (:exit (check/verdict [(check/addon-tools 'no.such/ctor)])))))
