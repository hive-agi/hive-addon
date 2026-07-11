(ns hive-addon.schema-test
  "Trifecta for the IAddon schema layer:
     golden      — representative good/bad values per value object,
     contract    — validate-addon over live reify instances,
     generative  — malli.generator over the cleanly-generatable schemas,
     completeness— a function schema exists for every protocol method."
  (:require [clojure.test :refer [deftest is are testing]]
            [malli.generator :as mg]
            [hive-addon.protocol :as proto]
            [hive-addon.schema :as s]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(defn- conforming-addon []
  (reify proto/IAddon
    (addon-id [_] "demo.addon")
    (addon-type [_] :native)
    (capabilities [_] #{:tools :health-reporting})
    (initialize! [_ _cfg] {:success? true :errors [] :metadata {:n 1}})
    (shutdown! [_] nil)
    (tools [_] [{:name "demo_tool" :description "d" :handler (fn [_] {})}])
    (schema-extensions [_] {"agent" {"role" {:type "string"}}})
    (health [_] {:status :ok :details {:uptime-ms 10}})
    (excluded-tools [_] #{"read_file"})
    (hooks [_] {:spawn/opts-overlay (fn [x] x)})))

(defn- broken-addon []
  (reify proto/IAddon
    (addon-id [_] "")                    ; violates [:string {:min 1}]
    (addon-type [_] :native)
    (capabilities [_] #{:tools})
    (initialize! [_ _cfg] {:success? true})
    (shutdown! [_] nil)
    (tools [_] [])
    (schema-extensions [_] [])
    (health [_] {:status :ok})
    (excluded-tools [_] #{})
    (hooks [_] {})))

;; legacy addon: omits the two OPTIONAL methods (excluded-tools, hooks) entirely
(defn- legacy-addon []
  (reify proto/IAddon
    (addon-id [_] "legacy.addon")
    (addon-type [_] :external)
    (capabilities [_] #{:tools})
    (initialize! [_ _cfg] {:success? true})
    (shutdown! [_] nil)
    (tools [_] [])
    (schema-extensions [_] [])
    (health [_] {:status :degraded})))

;; ---------------------------------------------------------------------------
;; Golden — value objects
;; ---------------------------------------------------------------------------

(deftest value-object-schemas
  (testing "good values validate"
    (are [sch x] (true? (s/validate sch x))
      s/AddonId          "hive.knowledge"
      s/AddonType        :native
      s/AddonType        :mcp-bridge
      s/HealthStatus     :degraded
      s/CapabilitySet    #{:tools :schema}
      s/HealthReport     {:status :ok}
      s/HealthReport     {:status :down :details {:reason "x"}}
      s/InitResult       {:success? true}
      s/InitResult       {:success? false :errors ["boom"]}
      s/ToolDef          {:name "t"}
      s/Tools            [{:name "t" :handler identity}]
      s/SchemaExtensions {}
      s/SchemaExtensions []
      s/SchemaExtensions {"agent" {"role" {}}}
      s/HookMap          {:cu/a identity :op-schema/carto {}}
      s/ExcludedTools    #{"read_file"}))
  (testing "bad values are rejected"
    (are [sch x] (false? (s/validate sch x))
      s/AddonId          ""
      s/AddonType        :bogus
      s/HealthStatus     :sideways
      s/HealthReport     {:status :nope}
      s/HealthReport     {}
      s/InitResult       {:success? "yes"}
      s/ToolDef          {:name ""}
      s/HookMap          {"str-key" 1}          ; unqualified key
      s/ExcludedTools    #{""})))

(deftest enums-track-protocol
  (testing "enum schemas are derived from the protocol constants (no drift)"
    (is (= proto/valid-addon-types (set (rest s/AddonType))))
    (is (= proto/health-statuses   (set (rest s/HealthStatus))))))

;; ---------------------------------------------------------------------------
;; Contract — validate-addon over live reifies
;; ---------------------------------------------------------------------------

(deftest validate-addon-conforms
  (let [a   (conforming-addon)
        res (s/validate-addon a)]
    (is (r/ok? res))
    (is (identical? a (:ok res)))))

(deftest validate-addon-detects-violation
  (let [res (s/validate-addon (broken-addon))]
    (is (r/err? res))
    (is (= :addon/contract-violation (:error res)))
    (is (= :addon/id (:method res)))))

(deftest validate-addon-skips-legacy-optionals
  (testing "an addon omitting excluded-tools/hooks still conforms (legacy path)"
    (is (r/ok? (s/validate-addon (legacy-addon))))))

;; ---------------------------------------------------------------------------
;; Result bridge
;; ---------------------------------------------------------------------------

(deftest result-bridge
  (testing "validate* returns a hive-dsl Result with a qualified-keyword category"
    (is (r/ok? (s/validate* s/InitResult {:success? true})))
    (let [e (s/validate* s/InitResult {:success? "yes"})]
      (is (r/err? e))
      (is (qualified-keyword? (:error e)))
      (is (= :addon/schema-violation (:error e)))
      (is (some? (:explanation e))))
    (testing "custom category is honored"
      (is (= :addon/my-cat (:error (s/validate* s/AddonType :bogus :addon/my-cat)))))))

;; ---------------------------------------------------------------------------
;; Generative — cleanly-generatable value objects (Tools/ToolDef carry fn? -> golden-only)
;; ---------------------------------------------------------------------------

(def ^:private generatable
  {:AddonType    s/AddonType    :HealthStatus s/HealthStatus :HealthReport s/HealthReport
   :InitResult   s/InitResult   :CapabilitySet s/CapabilitySet :ExcludedTools s/ExcludedTools
   :HookMap      s/HookMap      :SchemaExtensions s/SchemaExtensions :AddonId s/AddonId})

(deftest generative-round-trip
  (testing "generated samples validate against their own schema"
    (doseq [[nm sch] generatable]
      (dotimes [i 25]
        (let [x (mg/generate sch {:registry s/registry :size 10 :seed (inc i)})]
          (is (s/validate sch x)
              (str nm " generated value failed validation: " (pr-str x))))))))

(deftest method-schemas-cover-protocol
  (testing "there is a function schema for every one of the 10 IAddon methods"
    (let [method-syms (set (map #(symbol "hive-addon.protocol" (name %))
                                '[addon-id addon-type capabilities initialize! shutdown!
                                  tools schema-extensions health excluded-tools hooks]))]
      (is (= method-syms (set (keys s/method-schemas)))))))
