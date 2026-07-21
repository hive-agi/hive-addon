(ns hive-addon.mount.compose-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.set :as set]
            [hive-addon.mount.compose :as compose]
            [hive-addon.mount.solve :as solve]
            [hive-addon.mount.port :as port]
            [hive-addon.protocol :as proto]
            [hive-dsl.result :as r]))

;; SPDX-License-Identifier: MIT

;; -----------------------------------------------------------------------------
;; In-memory fakes — ctor vars resolvable by requiring-resolve (isolation axiom)
;; -----------------------------------------------------------------------------

(def captured (atom {}))
(def compose-init-attempts (atom 0))

(use-fixtures :each (fn [t]
                      (reset! captured {})
                      (reset! compose-init-attempts 0)
                      (t)))

(defn- fake-addon [id config]
  (swap! captured assoc id config)
  (reify proto/IAddon
    (addon-id [_] id)
    (addon-type [_] :native)
    (capabilities [_] #{})
    (initialize! [_ _config] {:success? true :errors []})
    (shutdown! [_] nil)
    (tools [_] [])
    (schema-extensions [_] {})
    (health [_] {:status :ok})
    (excluded-tools [_] #{})
    (hooks [_] {})))

(defn ctor-a [config] (fake-addon "a" config))
(defn ctor-b [config] (fake-addon "b" config))
(defn ctor-c [config] (fake-addon "c" config))

(defn ctor-flaky [config]
  (let [id (:addon/id config)]
    (reify proto/IAddon
      (addon-id [_] id)
      (addon-type [_] :native)
      (capabilities [_] #{})
      (initialize! [_ _]
        {:success? (>= (swap! compose-init-attempts inc) 2)
         :errors ["not ready"]})
      (shutdown! [_] nil)
      (tools [_] [])
      (schema-extensions [_] {})
      (health [_] {:status :ok})
      (excluded-tools [_] #{})
      (hooks [_] {}))))

(defn- spec [id ctor & {:as extra}]
  (merge {:addon/id      id
          :addon/type    :native
          :addon/init-ns "hive-addon.mount.compose-test"
          :addon/init-fn ctor}
         extra))

(def specs-abc
  [(spec "a" "ctor-a") (spec "b" "ctor-b") (spec "c" "ctor-c")])

(defn- lib [n] (symbol "com.acme" (str "addon-" n)))

(defn- plug [id & {:as extra}]
  (merge {:source {:mvn/version "1.0.0"} :addon/id id} extra))

(defn- layer [plugs & {:as extra}]
  {:id :test :config (merge {:iaddon/plugs plugs} extra)})

;; -----------------------------------------------------------------------------
;; select-specs — pure filter
;; -----------------------------------------------------------------------------

(deftest select-specs-opt-out
  (testing "default: plug only removes; unmentioned specs are kept"
    (let [idx {:dropped-ids #{"b"} :selected-ids #{"a"}
               :dropped-reasons {"b" :disabled} :config-by-id {} :unjoinable []}
          {:keys [specs dropped]} (compose/select-specs specs-abc idx {:strict-select false})]
      (is (= #{"a" "c"} (into #{} (map :addon/id) specs)))
      (is (= {"b" :disabled} dropped)))))

(deftest select-specs-strict
  (testing "strict: keep only explicitly selected ids"
    (let [idx {:dropped-ids #{} :selected-ids #{"a"}
               :dropped-reasons {} :config-by-id {} :unjoinable []}
          {:keys [specs dropped]} (compose/select-specs specs-abc idx {:strict-select true})]
      (is (= ["a"] (mapv :addon/id specs)))
      (is (= {"b" :not-selected "c" :not-selected} dropped)))))

(deftest select-specs-selected-wins-over-dropped
  (testing "default: an addon-id in BOTH selected and dropped (two libs, same id) is KEPT"
    (let [idx {:dropped-ids #{"a"} :selected-ids #{"a"}
               :dropped-reasons {"a" {:capability-lost-to 'com.acme/other}}
               :config-by-id {} :unjoinable []}
          {:keys [specs dropped]} (compose/select-specs specs-abc idx {:strict-select false})]
      (is (contains? (into #{} (map :addon/id) specs) "a"))
      (is (not (contains? dropped "a"))))))

;; -----------------------------------------------------------------------------
;; compose-plan — plug resolve + select + solve
;; -----------------------------------------------------------------------------

(deftest compose-plan-opt-out-disable
  (testing "disabled plug drops its spec; spec with no plug entry is kept"
    (let [layers [(layer {(lib "a") (plug "a")
                          (lib "b") (plug "b" :enabled? false)})]
          res    (compose/compose-plan specs-abc layers)]
      (is (r/ok? res))
      (let [{:keys [selected-ids dropped plan]} (:ok res)]
        (is (= #{"a" "c"} selected-ids))
        (is (= :disabled (get dropped "b")))
        (is (= #{"a" "c"} (into #{} (map :addon/id) (:ordered plan))))))))

(deftest compose-plan-strict-select
  (testing "strict-select keeps only the plug-selected id"
    (let [layers [(layer {(lib "a") (plug "a")})]
          res    (compose/compose-plan specs-abc layers {:strict-select true})]
      (is (r/ok? res))
      (let [{:keys [selected-ids dropped]} (:ok res)]
        (is (= #{"a"} selected-ids))
        (is (= {"b" :not-selected "c" :not-selected} dropped))))))

(deftest compose-plan-capability-arbitration
  (testing "capability :prefer drops the losing provider's addon-id"
    (let [layers [(layer {(lib "a") (plug "a" :capability :cartography)
                          (lib "b") (plug "b" :capability :cartography)}
                         :iaddon/capabilities {:cartography {:prefer (lib "a")}})]
          res    (compose/compose-plan [(spec "a" "ctor-a") (spec "b" "ctor-b")] layers)]
      (is (r/ok? res))
      (let [{:keys [selected-ids dropped]} (:ok res)]
        (is (= #{"a"} selected-ids))
        (is (contains? dropped "b"))))))

(deftest compose-plan-shared-addon-id-winner-kept
  (testing "two libs share an addon-id; the arbitration winner is kept, not dropped"
    (let [layers [(layer {(lib "a1") (plug "shared" :capability :cartography)
                          (lib "a2") (plug "shared" :capability :cartography)}
                         :iaddon/capabilities {:cartography {:prefer (lib "a1")}})]
          res    (compose/compose-plan [(spec "shared" "ctor-a")] layers)]
      (is (r/ok? res))
      (let [{:keys [selected-ids dropped plan]} (:ok res)]
        (is (= #{"shared"} selected-ids))
        (is (not (contains? dropped "shared")))
        (is (= ["shared"] (mapv :addon/id (:ordered plan))))))))

(deftest compose-plan-unjoinable
  (testing "a plug lacking :addon/id lands in :unjoinable, never crashes"
    (let [lx     (lib "x")
          layers [(layer {(lib "a") (plug "a")
                          lx        {:source {:mvn/version "1.0.0"}}})]
          res    (compose/compose-plan [(spec "a" "ctor-a")] layers)]
      (is (r/ok? res))
      (let [{:keys [selected-ids unjoinable]} (:ok res)]
        (is (= #{"a"} selected-ids))
        (is (some #{lx} unjoinable))))))

(deftest compose-plan-selected-absent
  (testing "selecting an addon-id with no discovered spec is reported, not an error"
    (let [layers [(layer {(lib "a") (plug "a")
                          (lib "z") (plug "z")})]
          res    (compose/compose-plan [(spec "a" "ctor-a")] layers)]
      (is (r/ok? res))
      (let [{:keys [selected-ids selected-absent]} (:ok res)]
        (is (= #{"a"} selected-ids))
        (is (contains? selected-absent "z"))))))

(deftest compose-plan-subset-topo-order
  (testing "solved order over the selected subset respects dependencies"
    (let [specs  [(spec "a" "ctor-a" :addon/dependencies #{"b"})
                  (spec "b" "ctor-b")
                  (spec "c" "ctor-c")]
          layers [(layer {(lib "c") (plug "c" :enabled? false)})]
          res    (compose/compose-plan specs layers)]
      (is (r/ok? res))
      (let [{:keys [plan]} (:ok res)]
        (is (= ["b" "a"] (mapv :addon/id (:ordered plan))))
        (is (not (contains? (into #{} (map :addon/id) (:ordered plan)) "c")))))))

;; -----------------------------------------------------------------------------
;; compose! — mount into a host
;; -----------------------------------------------------------------------------

(deftest compose!-config-merge
  (testing "build.edn per-addon :config merges on top of and WINS over the base"
    (let [host   (port/atom-mount-host)
          specs  [(spec "a" "ctor-a" :addon/config {:k 0 :base true})]
          layers [(layer {(lib "a") (plug "a" :config {:k 1})})]
          res    (compose/compose! specs layers host)]
      (is (r/ok? res))
      (is (:ok? (:report (:ok res))))
      (let [cfg (get @captured "a")]
        (is (= 1 (:k cfg)) "build.edn value overrides the base default")
        (is (= true (:base cfg)) "base config keys are preserved")))))

(deftest compose!-back-compat-mount-all
  (testing "no plug layers ⇒ mount every discovered spec in solve order"
    (let [host        (port/atom-mount-host)
          res         (compose/compose! specs-abc [] host)
          solve-order (mapv :addon/id (:ordered (solve/solve specs-abc)))]
      (is (r/ok? res))
      (let [report (:report (:ok res))]
        (is (:ok? report))
        (is (= solve-order (:order report)))
        (is (= #{"a" "b" "c"} (set (keys @captured))))))))

(deftest compose-threads-init-retry-and-event-opts
  (let [events (atom [])
        flaky (spec "flaky" "ctor-flaky"
                    :addon/config {:addon/id "flaky"}
                    :addon/init-retry {:max-attempts 2
                                       :initial-delay-ms 0
                                       :max-delay-ms 0
                                       :backoff-factor 2})
        result (compose/compose! [flaky] [] (port/atom-mount-host)
                                 {:sleep-fn (constantly nil)
                                  :on-event #(swap! events conj %)})]
    (is (r/ok? result))
    (is (get-in result [:ok :report :ok?]))
    (is (= 2 @compose-init-attempts))
    (is (some #(= :mount/init-retry (:event %)) @events))))

;; -----------------------------------------------------------------------------
;; read-layers — filesystem + validation
;; -----------------------------------------------------------------------------

(defn- temp-edn [content]
  (let [f (java.io.File/createTempFile "iaddon" ".edn")]
    (.deleteOnExit f)
    (spit f content)
    (.getAbsolutePath f)))

(deftest read-layers-missing-skipped
  (testing "a missing path is skipped, not an error"
    (let [{:keys [layers errors]} (compose/read-layers ["/no/such/build-xyz.edn"])]
      (is (= [] layers))
      (is (= [] errors)))))

(deftest read-layers-malformed
  (testing "malformed EDN becomes an :errors entry, never throws"
    (let [p (temp-edn "{:iaddon/plugs {")
          {:keys [layers errors]} (compose/read-layers [p])]
      (is (= [] layers))
      (is (= 1 (count errors)))
      (is (= p (:path (first errors)))))))

(deftest read-layers-valid
  (testing "a valid iaddon.edn parses into a layer"
    (let [p (temp-edn (pr-str {:iaddon/plugs {(lib "a") {:source {:mvn/version "1.0.0"}
                                                          :addon/id "a"}}}))
          {:keys [layers errors]} (compose/read-layers [p])]
      (is (= [] errors))
      (is (= 1 (count layers)))
      (is (= p (:id (first layers))))
      (is (contains? (:config (first layers)) :iaddon/plugs)))))
