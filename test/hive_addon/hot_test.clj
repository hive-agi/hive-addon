(ns hive-addon.hot-test
  "Behavioural suite for the IAddon hot-reload bridge.

   The claim under test is not \"reload runs without throwing\" — it is that a
   reload REPLACES instances (new generation), CASCADES to dependents that were
   holding the old instance, and PRESERVES the sibling injection those dependents
   were originally mounted with. Generation counters in hive-addon.hot-fixture
   are what make those three separable."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-addon.hot :as hot]
            [hive-addon.hot.cascade :as cascade]
            [hive-addon.hot.schema :as hs]
            [hive-addon.hot.source :as source]
            [hive-addon.hot.strategy :as strategy]
            [hive-addon.hot-fixture :as fx]
            [hive-addon.mount :as mount]
            [hive-addon.mount.port :as port]
            [hive-dsl.result :as r]
            [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def fixture-ns "hive-addon.hot-fixture")

(defn spec
  [id init-fn & {:as extra}]
  (merge {:addon/id id
          :addon/type :native
          :addon/init-ns fixture-ns
          :addon/init-fn init-fn}
         extra))

;; A -> B -> C : B depends on A, C depends on B.
(def spec-a (spec "probe.a" "make-a"))
(def spec-b (spec "probe.b" "make-b" :addon/dependencies #{"probe.a"}))
(def spec-c (spec "probe.c" "make-c" :addon/dependencies #{"probe.b"}))
(def chain-specs [spec-a spec-b spec-c])

(defn- reset-all! [f]
  (fx/reset-fixture!)
  (strategy/reset-strategies!)
  (try (f) (finally (strategy/reset-strategies!))))

(use-fixtures :each reset-all!)

(defn mount-chain!
  "Mount the A->B->C chain into a fresh in-memory host. Returns [host report]."
  ([] (mount-chain! chain-specs))
  ([specs]
   (let [host (mount/atom-mount-host)
         report (mount/mount! (mount/solve specs) host)]
     [host report])))

(defn generation-of
  [host id]
  (:generation (port/registered host id)))

;; =============================================================================
;; Pure cascade
;; =============================================================================

(deftest dependents-closes-forward-transitively
  (testing "seeds are included, and the closure reaches indirect dependents"
    (is (= #{"probe.a" "probe.b" "probe.c"} (cascade/dependents chain-specs #{"probe.a"})))
    (is (= #{"probe.b" "probe.c"} (cascade/dependents chain-specs #{"probe.b"})))
    (is (= #{"probe.c"} (cascade/dependents chain-specs #{"probe.c"}))))
  (testing "a leaf reload does not drag in its dependencies"
    (is (not (contains? (cascade/dependents chain-specs #{"probe.c"}) "probe.a")))))

(deftest dependents-follows-capability-edges
  (testing "an addon requiring a capability is a dependent of its provider"
    (let [provider (spec "probe.a" "make-a" :addon/capabilities #{:storage})
          consumer (spec "probe.b" "make-b" :addon/requires-capabilities #{:storage})
          specs    [provider consumer]]
      (is (= #{"probe.a" "probe.b"} (cascade/dependents specs #{"probe.a"})))
      (is (= #{"probe.b"} (cascade/dependents specs #{"probe.b"}))))))

(deftest affected-plan-keeps-global-topological-order
  (testing "the slice is ordered deps-before-dependents"
    (let [plan (cascade/affected-plan chain-specs #{"probe.a"})]
      (is (= ["probe.a" "probe.b" "probe.c"] (mapv :addon/id (:ordered plan))))))
  (testing "an unaffected dependency is excluded but does not reorder the rest"
    (let [plan (cascade/affected-plan chain-specs #{"probe.b"})]
      (is (= ["probe.b" "probe.c"] (mapv :addon/id (:ordered plan)))))))

(deftest affected-plan-terminates-on-cycles
  (testing "a cycle does not hang the closure"
    (let [x (spec "probe.a" "make-a" :addon/dependencies #{"probe.b"})
          y (spec "probe.b" "make-b" :addon/dependencies #{"probe.a"})]
      (is (= #{"probe.a" "probe.b"} (cascade/dependents [x y] #{"probe.a"}))))))

(deftest seeds-for-ns-seeds-every-addon-sharing-a-constructor-namespace
  (testing "two addons built from one namespace both seed"
    (let [s1 (spec "probe.one" "make-shared")
          s2 (spec "probe.two" "make-shared")]
      (is (= #{"probe.one" "probe.two"} (cascade/seeds-for-ns [s1 s2] fixture-ns)))))
  (testing "an unknown namespace seeds nothing"
    (is (= #{} (cascade/seeds-for-ns chain-specs "no.such.ns")))))

;; =============================================================================
;; Source classification — the :local/root vs jar distinction
;; =============================================================================

(deftest source-classifies-a-directory-backed-namespace-as-reloadable
  (let [src (source/resolve-source fixture-ns)]
    (is (= :directory (:hot/source-kind src)))
    (is (true? (:hot/reloadable? src)))
    (testing "the source dir is the classpath ROOT, which is what clj-reload watches"
      (is (string? (:hot/source-dir src)))
      (is (not (re-find #"hive_addon" (:hot/source-dir src)))))))

(deftest source-classifies-a-missing-namespace-as-absent
  (let [src (source/resolve-source "definitely.not.a.real.namespace")]
    (is (= :absent (:hot/source-kind src)))
    (is (false? (:hot/reloadable? src)))))

(deftest watchable-dirs-collects-only-reloadable-sources
  (let [dirs (source/watchable-dirs [spec-a (spec "probe.x" "make-a"
                                                  :addon/init-ns "definitely.not.real")])]
    (is (= 1 (count dirs)))
    (is (every? string? dirs))))

;; =============================================================================
;; Strategy selection — the OCP seam
;; =============================================================================

(defn- ctx-for [spec] {:hot/source (source/spec-source spec) :hot/specs chain-specs})

(deftest default-selection-is-remount
  (is (= :remount (strategy/-strategy-id
                   (:ok (strategy/select spec-a (ctx-for spec-a)))))))

(deftest unreloadable-source-selects-restart-required
  (let [s (spec "probe.jarred" "make-a" :addon/init-ns "definitely.not.real")]
    (is (= :restart-required
           (strategy/-strategy-id (:ok (strategy/select s (ctx-for s))))))))

(deftest a-declared-strategy-is-honoured-verbatim
  (let [s (spec "probe.a" "make-a" :addon/reload-strategy :in-place)]
    (is (= :in-place (strategy/-strategy-id (:ok (strategy/select s (ctx-for s)))))))
  (testing "a declared strategy overrides what -applies? would have chosen"
    (let [s (spec "probe.jarred" "make-a"
                  :addon/init-ns "definitely.not.real"
                  :addon/reload-strategy :inert)]
      (is (= :inert (strategy/-strategy-id (:ok (strategy/select s (ctx-for s)))))))))

(deftest an-unknown-declared-strategy-errors-rather-than-falling-back
  (testing "asking for special handling and silently getting generic handling is worse than refusing"
    (let [s   (spec "probe.a" "make-a" :addon/reload-strategy :no-such-strategy)
          res (strategy/select s (ctx-for s))]
      (is (r/err? res))
      (is (= :hot/unknown-strategy (:error res)))
      (is (= :no-such-strategy (:declared res))))))

(defrecord RecordingStrategy [calls]
  strategy/IReloadStrategy
  (-strategy-id [_] :probe/recording)
  (-applies? [_ _spec _ctx] true)
  (-reload! [_ spec ctx]
    (swap! calls conj (:addon/id spec))
    {:hot/trigger (:hot/trigger ctx :manual)
     :hot/strategy :probe/recording
     :hot/seeds (set (:hot/seeds ctx))
     :hot/affected [(:addon/id spec)]
     :hot/torn-down []
     :teardown/data-preserved? true
     :mounted []
     :ok? true}))

(deftest ocp-a-registered-strategy-wins-without-editing-the-chain
  (let [calls (atom [])]
    (strategy/register-strategy! (->RecordingStrategy calls))
    (testing "it precedes the built-ins"
      (is (= :probe/recording
             (strategy/-strategy-id (:ok (strategy/select spec-a (ctx-for spec-a)))))))
    (testing "and it actually runs"
      (let [[host _] (mount-chain!)
            report (hot/reload-addon! host chain-specs "probe.a")]
        (is (:ok? report))
        (is (= :probe/recording (:hot/strategy report)))
        (is (= ["probe.a"] @calls))))
    (testing "reset restores the built-ins"
      (strategy/reset-strategies!)
      (is (= :remount
             (strategy/-strategy-id (:ok (strategy/select spec-a (ctx-for spec-a)))))))))

(deftest registering-the-same-strategy-twice-is-idempotent
  (let [calls (atom [])]
    (strategy/register-strategy! (->RecordingStrategy calls))
    (strategy/register-strategy! (->RecordingStrategy calls))
    (is (= 1 (count (filter #(= :probe/recording (strategy/-strategy-id %))
                            (strategy/installed-strategies)))))))

(deftest an-orphaned-strategy-is-filtered-not-thrown
  (testing "a registered object that no longer satisfies the protocol degrades to the built-ins"
    (strategy/install-strategies! nil)
    (swap! @#'strategy/overrides assoc :extra [{:not "a strategy"}])
    (is (= [:restart-required :in-place :inert :remount]
           (mapv strategy/-strategy-id (strategy/installed-strategies))))))

(deftest default-strategies-is-a-builder-so-it-tracks-the-live-protocol
  (testing "rebuilt per call — the defonce-holding-instances trap"
    (is (every? #(satisfies? strategy/IReloadStrategy %) (strategy/default-strategies)))
    (is (not (identical? (first (strategy/default-strategies))
                         (first (strategy/default-strategies)))))))

;; =============================================================================
;; Remount — the behaviour that matters
;; =============================================================================

(deftest remount-replaces-the-instance-rather-than-reinitializing-it
  (let [[host _] (mount-chain!)]
    (is (= 1 (generation-of host "probe.a")))
    (let [report (hot/reload-addon! host chain-specs "probe.a")]
      (is (:ok? report) (pr-str (:errors report)))
      (is (= :remount (:hot/strategy report)))
      (testing "a NEW object, not the old one re-initialized"
        (is (= 2 (generation-of host "probe.a")))))))

(deftest remount-cascades-to-dependents-holding-the-stale-instance
  (let [[host _] (mount-chain!)]
    (is (= [1 1 1] (mapv #(generation-of host %) ["probe.a" "probe.b" "probe.c"])))
    (let [report (hot/reload-addon! host chain-specs "probe.a")]
      (is (:ok? report) (pr-str (:errors report)))
      (testing "every dependent is rebuilt, in dependency order"
        (is (= ["probe.a" "probe.b" "probe.c"] (:hot/affected report)))
        (is (= [2 2 2] (mapv #(generation-of host %) ["probe.a" "probe.b" "probe.c"]))))))
  (testing "reloading a leaf leaves its dependencies untouched"
    (fx/reset-fixture!)
    (let [[host _] (mount-chain!)]
      (hot/reload-addon! host chain-specs "probe.c")
      (is (= [1 1 2] (mapv #(generation-of host %) ["probe.a" "probe.b" "probe.c"]))))))

(deftest remount-tears-down-in-reverse-order-before-rebuilding
  (let [[host _] (mount-chain!)
        report   (hot/reload-addon! host chain-specs "probe.a")]
    (is (= ["probe.c" "probe.b" "probe.a"] (:hot/torn-down report)))
    (testing "and the shutdowns really ran, newest-dependent first"
      (is (= [["probe.c" 1] ["probe.b" 1] ["probe.a" 1]]
             (mapv (fn [[_ id gen]] [id gen]) (fx/events-of :shutdown)))))
    (testing "no-nuke invariant survives"
      (is (true? (:teardown/data-preserved? report))))))

(deftest remount-preserves-sibling-injection
  (testing "a remounted dependent still receives its dependency instance"
    (let [[host _] (mount-chain!)]
      (hot/reload-addon! host chain-specs "probe.a")
      (let [inits (filterv (fn [[_ id gen _]] (and (= "probe.b" id) (= 2 gen)))
                           (fx/events-of :init))]
        (is (= 1 (count inits)))
        (is (= #{"probe.a"} (nth (first inits) 3)))))))

(deftest remounting-a-slice-still-injects-siblings-from-outside-the-slice
  (testing ":peer-specs — a capability provider outside the reload closure is still injected"
    (let [provider (spec "probe.a" "make-a" :addon/capabilities #{:storage})
          consumer (spec "probe.b" "make-b" :addon/requires-capabilities #{:storage})
          specs    [provider consumer]
          host     (mount/atom-mount-host)]
      (mount/mount! (mount/solve specs) host)
      (fx/reset-fixture!)
      ;; Reload only the consumer: the provider is a dependency, so it is NOT in
      ;; the slice. Its instance must still reach the consumer's config.
      (let [report (hot/reload-addon! host specs "probe.b")]
        (is (:ok? report) (pr-str (:errors report)))
        (is (= ["probe.b"] (:hot/affected report)))
        (let [[_ _ _ deps] (first (fx/events-of :init))]
          (is (= #{"probe.a"} deps)
              "the out-of-slice provider must survive in :mount/dependencies"))))))

(deftest remount-degrades-gracefully-when-a-constructor-fails
  (let [broken (spec "probe.a" "make-broken")
        specs  [broken spec-b]
        host   (mount/atom-mount-host)]
    (mount/mount! (mount/solve [spec-a spec-b]) host)
    (let [report (hot/reload-addon! host specs "probe.a")]
      (is (false? (:ok? report)))
      (testing "the failure is reported as data, not thrown"
        (is (seq (:errors report))))
      (testing "and no data was deleted"
        (is (true? (:teardown/data-preserved? report)))))))

(deftest reload-all-rebuilds-every-addon-in-order
  (let [[host _] (mount-chain!)
        report   (hot/reload-all! host chain-specs)]
    (is (:ok? report) (pr-str (:errors report)))
    (is (= ["probe.a" "probe.b" "probe.c"] (:hot/affected report)))
    (is (= [2 2 2] (mapv #(generation-of host %) ["probe.a" "probe.b" "probe.c"])))))

(deftest reloading-an-unknown-addon-refuses-by-value
  (let [[host _] (mount-chain!)
        report   (hot/reload-addon! host chain-specs "probe.nope")]
    (is (false? (:ok? report)))
    (is (seq (:errors report)))
    (testing "nothing was torn down"
      (is (empty? (:hot/torn-down report)))
      (is (= 1 (generation-of host "probe.a"))))))

;; =============================================================================
;; Namespace-triggered reload
;; =============================================================================

(deftest reload-namespace-seeds-every-addon-built-from-it
  (let [s1    (spec "probe.one" "make-shared" :addon/config {:probe/id "probe.one"})
        s2    (spec "probe.two" "make-shared" :addon/config {:probe/id "probe.two"})
        specs [s1 s2]
        host  (mount/atom-mount-host)]
    (mount/mount! (mount/solve specs) host)
    (let [report (hot/reload-namespace! host specs fixture-ns
                                        {:reload-ns! (fn [_] {:loaded []})})]
      (is (:ok? report) (pr-str (:errors report)))
      (is (= #{"probe.one" "probe.two"} (:hot/seeds report)))
      (is (= :ns-reload (:hot/trigger report)))
      (is (= fixture-ns (:hot/changed-ns report))))))

(deftest a-namespace-callback-does-not-reload-namespaces-again
  (testing "clj-reload already did the ns work before the component callback fires"
    (let [called (atom 0)
          [host _] (mount-chain!)]
      (hot/reload-namespace! host chain-specs fixture-ns
                             {:reload-ns! (fn [_] (swap! called inc) {:loaded []})})
      (is (zero? @called)))))

(deftest a-manual-reload-does-reload-namespaces-first
  (let [called (atom [])
        [host _] (mount-chain!)]
    (hot/reload-addon! host chain-specs "probe.a"
                       {:reload-ns! (fn [nss] (swap! called conj (vec nss)) {:loaded nss})})
    (is (= 1 (count @called)))
    (testing "every affected addon's namespace is offered to the reloader"
      (is (= [fixture-ns] (first @called))))))

(deftest a-failing-namespace-reload-aborts-before-teardown
  (testing "code that will not compile must not take the running system down with it"
    (let [[host _] (mount-chain!)
          report (hot/reload-addon! host chain-specs "probe.a"
                                    {:reload-ns! (fn [_] {:loaded [] :failed 'probe.ns
                                                          :error "syntax error"})})]
      (is (false? (:ok? report)))
      (is (empty? (:hot/torn-down report)))
      (testing "the old instances are untouched"
        (is (= [1 1 1] (mapv #(generation-of host %) ["probe.a" "probe.b" "probe.c"])))))))

(defn- mount-with-outsider!
  "Mount the A->B->C chain plus one addon whose constructor lives in a DIFFERENT
   namespace. Returns [host specs outsider-ns].

   Every other probe in this suite is built from hive-addon.hot-fixture, so
   without a second namespace no test can tell an addon inside the reloaded
   slice from one outside it."
  []
  (let [outsider-ns "hive-addon.hot-fixture-other"
        outsider    (spec "probe.out" "make-other"
                          :addon/init-ns outsider-ns
                          :addon/config {:probe/id "probe.out"})
        specs       (conj chain-specs outsider)
        host        (mount/atom-mount-host)]
    (mount/mount! (mount/solve specs) host)
    [host specs outsider-ns]))

(deftest a-reload-that-loads-another-addons-ns-remounts-that-addon-too
  (testing "the injected reloader reloads the IMAGE, not the requested slice, so
            an addon outside the slice can have its constructor namespace
            reloaded — and remounting only the seed would leave it holding an
            instance built from the superseded code"
    (let [[host specs other-ns] (mount-with-outsider!)]
      (is (= [1 1] (mapv #(generation-of host %) ["probe.a" "probe.out"])))
      (let [report (hot/reload-addon! host specs "probe.a"
                                      {:reload-ns! (fn [_] {:loaded [fixture-ns other-ns]})})]
        (is (:ok? report) (pr-str (:errors report)))
        (testing "the addon the reload dragged in is NAMED, not silently included"
          (is (= #{"probe.out"} (:hot/widened report))))
        (testing "and it is actually rebuilt, not left stale"
          (is (contains? (set (:hot/affected report)) "probe.out"))
          (is (= 2 (generation-of host "probe.out"))))
        (testing ":hot/seeds still reports what was ASKED for"
          (is (= #{"probe.a"} (:hot/seeds report))))
        (is (nil? (hs/humanize-errors hs/RemountReport report))
            (pr-str (hs/humanize-errors hs/RemountReport report)))))))

(deftest a-reload-that-stays-inside-the-slice-does-not-widen
  (testing "the control: :hot/widened is ABSENT, so \"nothing else changed\" is
            distinguishable from \"other addons were dragged in\""
    (let [[host specs _] (mount-with-outsider!)
          report (hot/reload-addon! host specs "probe.a"
                                    {:reload-ns! (fn [_] {:loaded [fixture-ns]})})]
      (is (:ok? report) (pr-str (:errors report)))
      (is (nil? (:hot/widened report)))
      (is (not (contains? (set (:hot/affected report)) "probe.out")))
      (testing "an addon nothing reloaded keeps the instance it had"
        (is (= 1 (generation-of host "probe.out")))))))

(deftest reloaded-namespaces-are-reported-as-strings-whatever-the-reloader-answers
  (testing "clj-reload answers with SYMBOLS while :addon/init-ns and the report
            schema are both STRINGS — an unconverted symbol matches no addon and
            widening silently finds nothing"
    (let [[host specs other-ns] (mount-with-outsider!)
          report (hot/reload-addon!
                  host specs "probe.a"
                  {:reload-ns! (fn [_] {:loaded [(symbol fixture-ns) (symbol other-ns)]})})]
      (is (:ok? report) (pr-str (:errors report)))
      (is (= [fixture-ns other-ns] (:hot/ns-reloaded report)))
      (is (every? string? (:hot/ns-reloaded report)))
      (is (= #{"probe.out"} (:hot/widened report)))
      (is (nil? (hs/humanize-errors hs/RemountReport report))
          (pr-str (hs/humanize-errors hs/RemountReport report))))))

;; =============================================================================
;; Wiring, degradation, and the protocol interlock
;; =============================================================================

(deftest no-reload-pins-the-protocol-namespace
  (testing "the class-identity hazard is mechanized as data, not prose"
    (is (contains? hot/no-reload 'hive-addon.protocol))
    (is (contains? hot/no-reload 'hive-addon.hot.strategy))))

(deftest plan-reports-per-addon-strategy-and-source-without-effects
  (let [[host _] (mount-chain!)
        before (mapv #(generation-of host %) ["probe.a" "probe.b" "probe.c"])
        p (hot/plan host chain-specs)]
    (is (= 3 (count (:hot/registered p))))
    (is (every? #(= :remount (:hot/strategy-id %)) (:hot/registered p)))
    (is (seq (:hot/dirs p)))
    (is (contains? (:hot/no-reload p) 'hive-addon.protocol))
    (testing "no effects — dry-run parity"
      (is (= before (mapv #(generation-of host %) ["probe.a" "probe.b" "probe.c"]))))))

(deftest plan-separates-unreloadable-addons-from-registrable-ones
  (let [jarred (spec "probe.jarred" "make-a" :addon/init-ns "definitely.not.real")
        [host _] (mount-chain!)
        p (hot/plan host (conj chain-specs jarred))]
    (is (= 3 (count (:hot/registered p))))
    (is (= 1 (count (:hot/skipped p))))
    (is (= "probe.jarred" (:addon/id (first (:hot/skipped p)))))
    (is (= :absent (:hot/source-kind (first (:hot/skipped p)))))))

(deftest an-unreloadable-addon-refuses-with-actionable-guidance
  (let [jarred (spec "probe.jarred" "make-a" :addon/init-ns "definitely.not.real")
        specs  (conj chain-specs jarred)
        host   (mount/atom-mount-host)
        report (hot/reload-addon! host specs "probe.jarred")]
    (is (false? (:ok? report)))
    (is (= :restart-required (:hot/strategy report)))
    (testing "the message says what to do about it"
      (is (re-find #"local\.deps\.edn|Restart" (first (:errors report)))))))

(deftest inert-strategy-declares-an-addon-out-of-the-cascade
  (let [inert (spec "probe.a" "make-a" :addon/reload-strategy :inert)
        specs [inert spec-b spec-c]
        [host _] (mount-chain! specs)
        report (hot/reload-addon! host specs "probe.a")]
    (is (:ok? report))
    (is (= :inert (:hot/strategy report)))
    (testing "nothing was rebuilt"
      (is (empty? (:hot/affected report)))
      (is (= [1 1 1] (mapv #(generation-of host %) ["probe.a" "probe.b" "probe.c"]))))))

(deftest in-place-strategy-reloads-namespaces-and-keeps-the-instance
  (let [in-place (spec "probe.a" "make-a" :addon/reload-strategy :in-place)
        specs    [in-place spec-b spec-c]
        [host _] (mount-chain! specs)
        report   (hot/reload-addon! host specs "probe.a"
                                    {:reload-ns! (fn [nss] {:loaded nss})})]
    (is (:ok? report))
    (is (= :in-place (:hot/strategy report)))
    (testing "the instance is the SAME one — that is the point of :in-place"
      (is (= 1 (generation-of host "probe.a"))))))

(deftest mixed-strategies-each-run-under-their-own-policy
  (testing "one trigger, two seeds, two strategies — neither shadows the other"
    (let [inert (spec "probe.a" "make-a" :addon/reload-strategy :inert)
          other (spec "probe.d" "make-shared" :addon/config {:probe/id "probe.d"})
          specs [inert other]
          host  (mount/atom-mount-host)]
      (mount/mount! (mount/solve specs) host)
      (is (= 1 (generation-of host "probe.d")) "precondition: both mounted")
      (let [report (hot/reload-seeds! host specs #{"probe.a" "probe.d"}
                                      {:reload-ns! (fn [nss] {:loaded nss})})]
        (is (:ok? report) (pr-str (:errors report)))
        (is (= :hot/mixed (:hot/strategy report)))
        (testing "the inert one was left alone, the default one was rebuilt"
          (is (= 1 (generation-of host "probe.a")))
          (is (= 2 (generation-of host "probe.d"))))))))

(deftest status-reports-the-chain-and-the-interlock
  (let [s (hot/status)]
    (is (= [:restart-required :in-place :inert :remount] (:hot/strategies s)))
    (is (contains? (:hot/no-reload s) 'hive-addon.protocol))
    (is (boolean? (:hot/available? s)))))

;; =============================================================================
;; Schema conformance — reports are value objects, not ad-hoc maps
;; =============================================================================

(deftest reports-conform-to-their-schemas
  (let [[host _] (mount-chain!)]
    (testing "RemountReport"
      (let [report (hot/reload-addon! host chain-specs "probe.a")]
        (is (nil? (hs/humanize-errors hs/RemountReport report))
            (pr-str (hs/humanize-errors hs/RemountReport report)))))
    (testing "HotReport"
      (let [p (hot/plan host chain-specs)]
        (is (nil? (hs/humanize-errors hs/HotReport p))
            (pr-str (hs/humanize-errors hs/HotReport p)))))
    (testing "AddonSource"
      (let [src (source/spec-source spec-a)]
        (is (nil? (hs/humanize-errors hs/AddonSource src))
            (pr-str (hs/humanize-errors hs/AddonSource src)))))))

(deftest a-refusal-report-is-still-a-valid-report
  (let [host (mount/atom-mount-host)
        report (hot/reload-addon! host chain-specs "probe.nope")]
    (is (nil? (hs/humanize-errors hs/RemountReport report))
        (pr-str (hs/humanize-errors hs/RemountReport report)))))

;; =============================================================================
;; A host that refuses to replace — the regression that fixture hosts hide
;; =============================================================================

(defrecord RefusingHost [reg]
  port/IMountHost
  ;; Mimics hive-mcp.addons.core/register-addon!: a duplicate :addon/id is
  ;; REFUSED, the incumbent instance stays, and the refusal is not visible
  ;; through the port's return value. atom-mount-host overwrites instead, which
  ;; is why every other test in this file passes against a host that would
  ;; silently no-op a real remount.
  (register! [this addon]
    (let [id (proto/addon-id addon)]
      (when-not (contains? @reg id)
        (swap! reg assoc id addon)))
    this)
  (init! [_ id config] (when-let [a (get @reg id)] (proto/initialize! a config)))
  (shutdown! [_ id] (when-let [a (get @reg id)] (proto/shutdown! a)) nil)
  (registered [_ id] (get @reg id)))

(deftest a-host-that-refuses-to-replace-fails-loudly-instead-of-silently
  (testing "a remount that could not swap the instance must NOT report success"
    (let [reg  (atom {})
          host (->RefusingHost reg)]
      (mount/mount! (mount/solve chain-specs) host)
      (let [original (get @reg "probe.a")
            report   (hot/reload-addon! host chain-specs "probe.a"
                                        {:reload-ns! (fn [nss] {:loaded nss})})]
        (is (false? (:ok? report))
            "silently keeping the stale instance is the failure this guards")
        (is (some #(re-find #"did not replace" %) (:errors report))
            (pr-str (:errors report)))
        (testing "and the stale instance is still the one in the registry — reported, not hidden"
          (is (identical? original (get @reg "probe.a"))))))))

(deftest a-replacing-host-actually-swaps-the-instance
  (testing "the positive control: atom-mount-host replaces, so identity changes"
    (let [[host _] (mount-chain!)
          before   (port/registered host "probe.a")
          _        (hot/reload-addon! host chain-specs "probe.a")
          after    (port/registered host "probe.a")]
      (is (not (identical? before after)))
      (is (= 1 (:generation before)))
      (is (= 2 (:generation after))))))

(deftest a-host-that-does-not-track-instances-is-not-treated-as-refusing
  (testing "registered -> nil is 'not implemented', not 'refused'"
    (let [calls (atom [])
          host  (reify port/IMountHost
                  (register! [this addon] (swap! calls conj (proto/addon-id addon)) this)
                  (init! [_ _ _] {:success? true})
                  (shutdown! [_ _] nil)
                  (registered [_ _] nil))
          report (hot/reload-addon! host chain-specs "probe.a"
                                    {:reload-ns! (fn [nss] {:loaded nss})})]
      (is (:ok? report) (pr-str (:errors report)))
      (is (= ["probe.a" "probe.b" "probe.c"] @calls)))))

;; =============================================================================
;; The facade's own Capture-by-Var exposure
;; =============================================================================

(deftest facade-re-exports-resolve-through-the-var-not-a-snapshot
  (testing "a plain `def` alias captures the function object and ignores a rebind"
    ;; This is the shape that broke live: hive-addon.hot re-exported
    ;; strategy/installed-strategies with `def`, so reloading
    ;; hive-addon.hot.strategy left the facade calling the PREVIOUS fn, which
    ;; returned records built against the PREVIOUS protocol var — and every
    ;; call through the facade then threw "No implementation of method
    ;; :-strategy-id ... for class RestartRequiredStrategy".
    (with-redefs [strategy/installed-strategies (fn [] [::sentinel])]
      (is (= [::sentinel] (hot/installed-strategies))))
    (with-redefs [strategy/default-strategies (fn [] [::fresh])]
      (is (= [::fresh] (hot/default-strategies))))
    (with-redefs [cascade/dependents (fn [_ _] #{::closure})]
      (is (= #{::closure} (hot/dependents chain-specs #{"probe.a"}))))
    (with-redefs [source/watchable-dirs (fn [_] #{"/sentinel"})]
      (is (= #{"/sentinel"} (hot/watchable-dirs chain-specs))))))

(deftest facade-still-returns-live-protocol-satisfying-strategies
  (testing "the chain reached through the facade satisfies the CURRENT protocol"
    (is (every? #(satisfies? strategy/IReloadStrategy %) (hot/installed-strategies)))
    (is (= [:restart-required :in-place :inert :remount]
           (mapv strategy/-strategy-id (hot/installed-strategies))))))

;; =============================================================================
;; Misplaced mount options must not be silently dropped
;; =============================================================================

(deftest a-top-level-mount-option-is-folded-not-ignored
  (testing ":resolve-config passed at the top level still reaches mount!"
    ;; The live failure this guards: hive-mcp's tool passed :resolve-config at
    ;; the top level instead of under :mount-opts. mount! silently fell back to
    ;; resolve-config-default, so the remounted addon lost its config.edn merge
    ;; and its :runtime/ports — coming back :active, :success? true, and
    ;; DEGRADED, with its whole MCP subdomain gone.
    (let [[host _] (mount-chain!)
          seen (atom [])
          resolver (fn [spec] (swap! seen conj (:addon/id spec)) {:probe/marker true})
          report (hot/reload-addon! host chain-specs "probe.a"
                                    {:resolve-config resolver
                                     :reload-ns! (fn [nss] {:loaded nss})})]
      (is (:ok? report) (pr-str (:errors report)))
      (is (= ["probe.a" "probe.b" "probe.c"] @seen)
          "the custom resolver must have been consulted for every remounted addon")
      (testing "and the config it returned actually reached the addon"
        (is (true? (:probe/marker (:config (port/registered host "probe.a")))))))))

(deftest an-explicit-mount-opts-entry-wins-over-a-stray-one
  (let [[host _] (mount-chain!)
        report (hot/reload-addon! host chain-specs "probe.a"
                                  {:resolve-config (fn [_] {:probe/from :stray})
                                   :mount-opts {:resolve-config (fn [_] {:probe/from :explicit})}
                                   :reload-ns! (fn [nss] {:loaded nss})})]
    (is (:ok? report) (pr-str (:errors report)))
    (is (= :explicit (:probe/from (:config (port/registered host "probe.a")))))))

;; =============================================================================
;; Scoped namespace reload — what the reloader answers reaches the report
;; =============================================================================

(deftest the-reload-context-carries-the-seeds-source-roots
  (testing "the namespace reload is scoped to where the SEEDS' constructors live"
    (let [[host _] (mount-chain!)
          report   (hot/reload-addon! host chain-specs "probe.a"
                                      {:reload-ns! (fn [nss] {:loaded nss})})]
      (is (:ok? report) (pr-str (:errors report)))
      (is (= [(:hot/source-dir (source/resolve-source fixture-ns))]
             (:hot/roots report))))))

(deftest what-a-scoped-reloader-declined-or-dragged-is-named-in-the-report
  (let [[host _] (mount-chain!)
        report   (hot/reload-addon! host chain-specs "probe.a"
                                    {:reload-ns! (fn [nss]
                                                   {:loaded nss
                                                    :skipped ['other.session.ns]
                                                    :dragged ["dependent.ns"]
                                                    :unchanged? false
                                                    :multi-file {'shadowed.ns ["a.clj" "b.clj"]}})})]
    (is (:ok? report) (pr-str (:errors report)))
    (is (= ["other.session.ns"] (:hot/ns-skipped report)))
    (is (= ["dependent.ns"] (:hot/ns-dragged report)))
    (is (false? (:hot/ns-unchanged? report)))
    (is (= {"shadowed.ns" ["a.clj" "b.clj"]} (:hot/multi-file report)))
    (is (nil? (hs/humanize-errors hs/RemountReport report))
        (pr-str (hs/humanize-errors hs/RemountReport report)))))

(deftest a-remount-from-unchanged-source-says-so
  (let [[host _] (mount-chain!)
        report   (hot/reload-addon! host chain-specs "probe.a"
                                    {:reload-ns! (fn [_] {:loaded [] :unchanged? true})})]
    (is (:ok? report) (pr-str (:errors report)))
    (is (true? (:hot/ns-unchanged? report)))
    (testing "the instances are still rebuilt — the operator asked for a reload"
      (is (= 2 (generation-of host "probe.a"))))))

;; =============================================================================
;; A reload that claims a load that did not happen is refused
;; =============================================================================

(deftest a-reported-load-whose-constructor-did-not-change-is-refused
  (testing "the reloader says the namespace loaded; the constructor var is
            provably the same object; remounting would rebuild from OLD code
            and report success — so it is refused before any teardown"
    (let [[host _] (mount-chain!)
          report   (hot/reload-addon! host chain-specs "probe.a"
                                      {:reload-ns! (fn [nss] {:loaded nss})
                                       :ctor-probe (fn [_ns _fn] ::same-root)})]
      (is (false? (:ok? report)))
      (is (= [fixture-ns] (:hot/stale-ctors report)))
      (is (some #(re-find #"did not change" %) (:errors report)))
      (testing "nothing was torn down and every instance is the one that was live"
        (is (empty? (:hot/torn-down report)))
        (is (= [1 1 1] (mapv #(generation-of host %) ["probe.a" "probe.b" "probe.c"]))))
      (is (nil? (hs/humanize-errors hs/RemountReport report))
          (pr-str (hs/humanize-errors hs/RemountReport report))))))

(deftest a-probe-that-sees-a-changed-constructor-lets-the-remount-proceed
  (let [[host _] (mount-chain!)
        counter  (atom 0)
        report   (hot/reload-addon! host chain-specs "probe.a"
                                    {:reload-ns! (fn [nss] {:loaded nss})
                                     :ctor-probe (fn [_ns _fn] (swap! counter inc))})]
    (is (:ok? report) (pr-str (:errors report)))
    (is (nil? (:hot/stale-ctors report)))
    (is (= 2 (generation-of host "probe.a")))))

(deftest in-place-refuses-a-claimed-load-that-did-not-happen
  (let [s      (spec "probe.a" "make-a" :addon/reload-strategy :in-place)
        host   (mount/atom-mount-host)
        _      (mount/mount! (mount/solve [s]) host)
        report (hot/reload-addon! host [s] "probe.a"
                                  {:reload-ns! (fn [nss] {:loaded nss})
                                   :ctor-probe (fn [_ns _fn] ::same-root)})]
    (is (false? (:ok? report)))
    (is (= :in-place (:hot/strategy report)))
    (is (= [fixture-ns] (:hot/stale-ctors report)))
    (is (= 1 (generation-of host "probe.a")))))

(deftest the-callback-path-does-not-probe-because-the-load-already-happened
  (testing "a hive-hot component callback fires AFTER clj-reload loaded the
            namespace; there is no before-state to compare, so no refusal"
    (let [[host _] (mount-chain!)
          report   (hot/reload-namespace! host chain-specs fixture-ns
                                          {:ctor-probe (fn [_ns _fn] ::same-root)})]
      (is (:ok? report) (pr-str (:errors report)))
      (is (nil? (:hot/stale-ctors report)))
      (is (= 2 (generation-of host "probe.a"))))))

(deftest the-default-reloader-verifies-its-own-claim
  (testing "with the default reloader the probe is on: an unchanged fixture
            loads nothing, so nothing is stale and the remount proceeds"
    (let [[host _] (mount-chain!)
          report   (hot/reload-addon! host chain-specs "probe.a")]
      (is (:ok? report) (pr-str (:errors report)))
      (is (nil? (:hot/stale-ctors report)))
      (is (= 2 (generation-of host "probe.a"))))))
