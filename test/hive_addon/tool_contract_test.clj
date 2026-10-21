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

;; ── described-property check (opt-in) ───────────────────────────────────────

(def ^:private undescribed
  (assoc-in good [:inputSchema :properties "widget"] {:type "string"}))

(def ^:private req {:require-descriptions? true})

(deftest require-descriptions-promotes-the-warning-to-a-finding
  (is (= [] (contract/findings undescribed)))
  (is (= [[:inputSchema :properties "widget" :description]]
         (mapv :path (contract/findings undescribed req))))
  (is (contract/conforms? undescribed))
  (is (not (contract/conforms? undescribed req)))
  (is (contract/conforms? good req)))

(deftest require-descriptions-rejects-a-blank-or-non-string-description
  (is (seq (contract/findings (assoc-in good [:inputSchema :properties "widget" :description] "  ") req)))
  (is (seq (contract/findings (assoc-in good [:inputSchema :properties "widget" :description] 42) req))))

(deftest require-descriptions-keeps-structural-violations
  (let [t (assoc undescribed :name "")]
    (is (= #{[:name] [:inputSchema :properties "widget" :description]}
           (set (map :path (contract/findings t req)))))))

(deftest report-moves-warnings-into-violations-under-require-descriptions
  (is (= {:checked 1 :violations {} :warnings {"frob" [{:path [:inputSchema :properties "widget" :description]
                                                       :message "property has no description"}]}}
         (contract/report [undescribed])))
  (let [rep (contract/report [undescribed good] req)]
    (is (= #{"frob"} (set (keys (:violations rep)))))
    (is (empty? (:warnings rep)))))

(deftest assert-root-tools-fails-an-undescribed-property-only-when-required
  (is (= [:pass :pass] (captured-reports #(ct/assert-root-tools [undescribed]))))
  (is (= [:pass :fail] (captured-reports #(ct/assert-root-tools [undescribed] req)))))

(ct/deftest-root-tools the-macro-passes-require-descriptions [good] :require-descriptions? true)

(defn ->undescribed-addon [] (->StubAddon [undescribed]))

(deftest check-verdict-honours-require-descriptions
  (let [rs [(check/addon-tools `->undescribed-addon)]]
    (is (= 0 (:exit (check/verdict rs))))
    (is (= 1 (:exit (check/verdict rs req))))))

(deftest check-parse-args-splits-flags-from-ctors
  (is (= {:opts req :ctors ["a/b" "c/d"] :unknown []}
         (check/parse-args ["a/b" "--require-descriptions" "c/d"])))
  (is (= ["--nope"] (:unknown (check/parse-args ["--nope" "a/b"]))))
  (is (= {} (:opts (check/parse-args ["a/b"])))))

;; ── manifest-driven ctors (--manifests) ──────────────────────────────────

(deftest manifest-ctors-keeps-only-tools-manifests
  (is (= ["a.addon/addon-ctor" "b.init/create"]
         (check/manifest-ctors
          [{:addon/init-ns "b.init" :addon/init-fn "create" :addon/capabilities #{:tools}}
           {:addon/init-ns "a.addon" :addon/init-fn "addon-ctor" :addon/capabilities #{:tools :health-reporting}}
           {:addon/init-ns "c.host" :addon/init-fn "addon-ctor" :addon/capabilities #{:vessel}}
           {:addon/init-ns "a.addon" :addon/init-fn "addon-ctor" :addon/capabilities #{:tools}}
           {:addon/capabilities #{:tools}}]))))

(deftest with-manifest-ctors-is-inert-without-the-flag
  (is (= {:calls [["x/y" []]]}
         (check/with-manifest-ctors {:opts {} :ctors ["x/y"]}
           #(throw (ex-info "must not read" {}))))))

(deftest with-manifest-ctors-unions-explicit-and-found
  (is (= {:calls [["x/y" []] ["a/b" [{}]]]}
         (check/with-manifest-ctors
           {:opts {:manifests? true} :ctors ["x/y"]}
           (constantly {:manifests [{:addon/init-ns "a" :addon/init-fn "b" :addon/capabilities #{:tools}}
                                    {:addon/init-ns "x" :addon/init-fn "y" :addon/capabilities #{:tools}}]
                        :errors []})))))

(defn ->config-addon [config] (when (map? config) (->good-addon)))

(deftest addon-tools-passes-the-mounter-config-arg
  (is (= 1 (count (:tools (check/addon-tools `->config-addon [{}])))))
  (is (:error (check/addon-tools `->config-addon)))
  (is (re-find #"did not return an IAddon" (:error (check/addon-tools `->config-addon [nil])))))

(deftest with-manifest-ctors-refuses-an-empty-or-broken-gate
  (is (= 2 (:exit (check/with-manifest-ctors {:opts {:manifests? true} :ctors []}
                    (constantly {:manifests [] :errors []})))))
  (is (= 2 (:exit (check/with-manifest-ctors {:opts {:manifests? true} :ctors ["x/y"]}
                    (constantly {:manifests [] :errors ["bad.edn: EOF"]}))))))

(deftest read-manifests-reads-a-dir-and-reports-a-bad-file
  (let [dir (doto (.toFile (java.nio.file.Files/createTempDirectory
                            "manifests" (make-array java.nio.file.attribute.FileAttribute 0)))
              .deleteOnExit)]
    (spit (java.io.File. dir "a.edn") (pr-str {:addon/init-ns "a" :addon/init-fn "b" :addon/capabilities #{:tools}}))
    (spit (java.io.File. dir "z.edn") "{:unclosed")
    (spit (java.io.File. dir "readme.txt") "ignored")
    (let [{:keys [manifests errors]} (check/read-manifests (str dir))]
      (is (= ["a/b"] (check/manifest-ctors manifests)))
      (is (= 1 (count errors))))
    (is (= {:manifests [] :errors []} (check/read-manifests (str dir "/missing"))))))

(deftest parse-args-knows-the-manifests-flag
  (is (= {:opts {:manifests? true} :ctors [] :unknown []}
         (check/parse-args ["--manifests"]))))

(defspec require-descriptions-is-never-weaker 100
  (prop/for-all [t (mg/generator contract/RootToolDef)]
    (<= (count (contract/findings t)) (count (contract/findings t req)))))
