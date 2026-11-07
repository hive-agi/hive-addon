(ns hive-addon.lifecycle-composite-facet-test
  "Facets for the newly contracted pure cores of the lifecycle and the
   composite addon: hive-addon.lifecycle.policy (boot/activation/sweep
   decisions), hive-addon.lifecycle.surface (what a dormant addon is advertised
   by) and hive-addon.composite (member normalization, tool aggregation,
   health folding).

   Same two kinds of evidence as contract-spine-facet-test: trifectas pin
   behaviour with goldens and catch real regressions as mutants, and
   `contracts-hold-under-instrumentation` runs a battery through the m/=>
   contracts themselves, instrumented and restored per test."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.composite :as c]
            [hive-addon.lifecycle.policy :as pol]
            [hive-addon.lifecycle.surface :as sf]
            [hive-addon.protocol :as proto]
            [hive-schemas.test :as hst]
            [hive-test.trifecta :as tri]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private real-fold-health c/fold-health)
(def ^:private real-boot-partition pol/boot-partition)

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- leaf
  [id & {:keys [tools health] :or {tools [] health {:status :ok}}}]
  (reify proto/IAddon
    (addon-id [_] id)
    (addon-type [_] :native)
    (capabilities [_] #{})
    (initialize! [_ _] {:success? true})
    (shutdown! [_] nil)
    (tools [_] tools)
    (schema-extensions [_] [])
    (health [_] health)
    (excluded-tools [_] #{})
    (hooks [_] {})))

(def ^:private chain
  "c depends on b depends on a; d stands alone."
  [{:addon/id "a"}
   {:addon/id "b" :addon/dependencies #{"a"}}
   {:addon/id "c" :addon/dependencies #{"b"}}
   {:addon/id "d"}])

(defn- use-state [id phase policy last-used & {:as extra}]
  (merge {:addon/id id :phase phase :lifecycle {:policy policy :idle-ms 100}
          :last-used-ms last-used :in-flight 0 :activations 1 :evictions 0}
         extra))

;; =============================================================================
;; composite/fold-health
;; =============================================================================

(tri/deftrifecta fold-health
  hive-addon.composite/fold-health
  {:golden-path "test/golden/hive-addon/composite-fold-health.edn"
   :cases {:empty []
           :all-ok [:ok :ok]
           :all-down [:down :down]
           :mixed [:ok :down]
           :one-degraded [:ok :degraded]}
   :mutations [["any-down-is-down" (fn [ss] (if (some #{:down} ss) :down (real-fold-health ss)))]
               ["empty-is-down" (fn [ss] (if (empty? ss) :down (real-fold-health ss)))]
               ["mixed-is-ok" (fn [ss] (if (some #{:ok} ss) :ok (real-fold-health ss)))]]})

(hst/deftrifecta-from-schema fold-health-schema
  hive-addon.composite/fold-health
  {:in  [:vector [:enum :ok :degraded :down]]
   :out [:enum :ok :degraded :down]
   :mutation false
   :num-tests 100
   :rel (fn [in out]
          (cond (empty? in) (= :ok out)
                (every? #{:ok} in) (= :ok out)
                (every? #{:down} in) (= :down out)
                :else (= :degraded out)))})

;; =============================================================================
;; composite/tool-prefix — total, including the degenerate all-separator id
;; =============================================================================

(hst/deftrifecta-from-schema tool-prefix-schema
  hive-addon.composite/tool-prefix
  {:in  [:or :string :keyword :symbol :nil :int]
   :out [:re #"^([a-z0-9]+(_[a-z0-9]+)*)?$"]
   :mutation false
   :num-tests 200})

(deftest tool-prefix-is-total
  (is (= "" (c/tool-prefix ".")) "an all-separator id has no segment, and must not throw")
  (is (= "" (c/tool-prefix "./")))
  (is (= "chain_of_thought" (c/tool-prefix "hive.prompt.chain-of-thought"))))

;; =============================================================================
;; lifecycle.policy/boot-partition
;; =============================================================================

(tri/deftrifecta boot-partition
  hive-addon.lifecycle.policy/boot-partition
  {:golden-path "test/golden/hive-addon/boot-partition.edn"
   :apply? true
   :cases {:all-eager [chain {} (constantly {:tools [{:name "t"}]})]
           ;; a is lazy but b (eager) needs it: downgraded :required-by-eager.
           :lazy-dep-of-eager [chain {"a" {:policy :lazy}} (constantly {:tools [{:name "t"}]})]
           ;; c is a lazy leaf with a surface: dormant.
           :lazy-leaf [chain {"c" {:policy :lazy}} (constantly {:tools [{:name "t"}]})]
           ;; d is lazy with no surface: downgraded :no-surface.
           :lazy-no-surface [chain {"d" {:policy :lazy}} (constantly nil)]}
   :mutations [["lazy-always-dormant"
                (fn [specs lcs sof]
                  (let [lazy (set (keep (fn [[k v]] (when (= :lazy (:policy v)) k)) lcs))
                        r (real-boot-partition specs lcs sof)]
                    (-> r
                        (update :eager #(vec (remove lazy %)))
                        (assoc :dormant (vec (sort lazy)) :downgraded {}))))]
               ["no-downgrade-record"
                (fn [& args] (assoc (apply real-boot-partition args) :downgraded {}))]]})

;; =============================================================================
;; lifecycle.policy/sweep-plan — dependents first, never strands a dependent
;; =============================================================================

(def ^:private gen-sweep
  "chain specs + a UseState per id, each :active/:dormant, lazy/eager/pinned,
   used at 0 or 1000, swept at 2000 with idle-ms 100."
  (gen/let [phases   (gen/vector (gen/elements [:active :dormant]) 4)
            policies (gen/vector (gen/elements [:lazy :lazy :eager :pinned]) 4)
            used     (gen/vector (gen/elements [nil 0 1990]) 4)]
    {(:addon/id (chain 0)) (use-state "a" (phases 0) (policies 0) (used 0))
     "b" (use-state "b" (phases 1) (policies 1) (used 1))
     "c" (use-state "c" (phases 2) (policies 2) (used 2))
     "d" (use-state "d" (phases 3) (policies 3) (used 3))}))

(defspec sweep-never-evicts-under-a-live-dependent 200
  (prop/for-all [states gen-sweep]
    (let [{:keys [evict kept]} (pol/sweep-plan 2000 chain states)
          ev (set evict)]
      (and
       ;; every id lands in exactly one of :evict / :kept
       (= (set (keys states)) (into ev (keys kept)))
       (empty? (filter ev (keys kept)))
       ;; only lazy, active addons are evicted
       (every? #(let [st (states %)]
                  (and (= :lazy (get-in st [:lifecycle :policy]))
                       (= :active (:phase st))))
               evict)
       ;; anything evicted has no dependent that stays active
       (every? (fn [id]
                 (let [deps (disj (pol/dependent-closure chain #{id}) id)]
                   (every? #(or (ev %) (not= :active (:phase (states %)))) deps)))
               evict)
       ;; dependents-first: nothing evicted LATER depends on what went earlier
       (every? (fn [[i x]]
                 (every? #(not (contains? (disj (pol/dependent-closure chain #{x}) x) %))
                         (drop (inc i) evict)))
               (map-indexed vector evict))))))

;; =============================================================================
;; lifecycle.surface/resolve-surface
;; =============================================================================

(tri/deftrifecta resolve-surface
  hive-addon.lifecycle.surface/resolve-surface
  {:golden-path "test/golden/hive-addon/resolve-surface.edn"
   :apply? true
   :cases {:declared-wins [{:addon/surface {:tools [{:name "d" :handler inc}]}}
                           {:tools [{:name "l"}]}]
           :learned [{} {:commands {:carto ["read" "read" :write]}}]
           :blank-learned [{} {:tools [] :commands {"x" []}}]
           :malformed-declared [{:addon/surface {:tools "nope"}} nil]}
   :mutations [["learned-wins" (fn [spec learned]
                                 (if (sf/well-formed? learned)
                                   [(sf/normalize learned) :learned]
                                   (sf/resolve-surface spec learned)))]
               ["blank-is-learned" (fn [_ learned]
                                     (if (map? learned)
                                       [(sf/normalize learned) :learned]
                                       [nil :none]))]]})

(defspec normalize-is-idempotent 200
  (prop/for-all [cmds (gen/map (gen/elements ["carto" :memory "kg"])
                               (gen/vector (gen/elements ["a" "b" :c]) 0 4))
                 names (gen/vector (gen/elements ["t1" "t2"]) 0 3)]
    (let [n (sf/normalize {:tools (mapv (fn [x] {:name x :handler identity}) names)
                           :commands cmds})]
      (and (= n (sf/normalize n))
           (every? seq (vals (:commands n)))
           (not-any? :handler (:tools n))))))

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
    '[hive-addon.composite hive-addon.lifecycle.policy hive-addon.lifecycle.surface]
    (fn [n]
      (is (<= 28 n) "the three namespaces carry their contracts")
      (testing "hive-addon.composite"
        (let [a (leaf "x.alpha" :tools [{:name "render"}])
              b (leaf "y.beta" :tools [{:name "render"}] :health {:status :degraded})
              comp (c/composite "cat" {:members [a {:member/addon b :member/depends-on #{"x.alpha"}} 42]})]
          (is (nil? (c/normalize-member 42)))
          (is (= ["x.alpha" "y.beta"] (c/member-ids comp)))
          (is (= [] (c/live-members comp)))
          (is (= ["x.alpha" "y.beta"] (mapv :member/id (:ordered (c/order-members (c/members comp))))))
          (is (true? (:success? (proto/initialize! comp {}))))
          (is (= 2 (count (c/live-members comp))))
          (is (= #{"alpha_render" "beta_render"} (set (map :name (proto/tools comp)))))
          (is (= {"render" ["x.alpha" "y.beta"]}
                 (:collisions (c/aggregate-tools (c/live-members comp) :flat nil))))
          (is (= :degraded (:status (proto/health comp))))
          (is (= {:name "render"} (c/qualify-tool "" {:name "render"})))
          (is (= [] (c/member-ids 1)))
          (is (false? (c/composite? a)))
          (proto/shutdown! comp)))
      (testing "hive-addon.lifecycle.policy"
        (is (= {:policy :lazy :idle-ms 5}
               (pol/resolve-lifecycle {:addon/lifecycle {:policy :lazy :idle-ms -1}}
                                      {:idle-ms 5} {:policy :bogus})))
        (is (= #{"a" "b" "c"} (pol/dependency-closure chain #{"c"})))
        (is (= #{"a" "b" "c"} (pol/dependent-closure chain ["a"])))
        (is (= ["a" "b"] (pol/activation-ids chain "b" (constantly false))))
        (is (false? (pol/lazy-permitted? nil)))
        (is (= ["a" "b" "c" "d"] (:eager (pol/boot-partition chain (pol/resolve-all chain {}) (constantly nil)))))
        (is (= ["c"] (:evict (pol/sweep-plan 2000 chain
                                             {"c" (use-state "c" :active :lazy 0)
                                              "b" (use-state "b" :active :eager 0)})))))
      (testing "hive-addon.lifecycle.surface"
        (is (true? (sf/blank? nil)))
        (is (false? (sf/well-formed? "x")))
        (is (nil? (sf/declared {})))
        (is (= {:tools [{:name "t"}] :commands {"carto" ["read"]}}
               (sf/observed "me" [{:name "t"}] {"carto" {"read" {:addon "me"} "w" {:addon "o"}}})))
        (is (= {:name "t"} (sf/tool-decl {:name "t" :handler inc})))))))

(deftest a-misstated-contract-is-caught
  ;; Non-vacuity: instrumentation must reject a call outside the domain.
  ;; dependency-closure maps over the specs, and the spine's Specs is an open
  ;; map collection on purpose, so the out-of-domain probe is a NON-collection
  ;; spec argument, not a malformed entry.
  (with-instrumented
    '[hive-addon.lifecycle.policy]
    (fn [_]
      (is (thrown? clojure.lang.ExceptionInfo
                   (pol/dependency-closure "not-a-spec-coll" #{"a"}))))))
