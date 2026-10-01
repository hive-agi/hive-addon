(ns hive-addon.composite-test
  "The composite's contract: one addon whose members are addons, and which is
   itself a legal member of another composite."
  (:require [clojure.test :refer [deftest testing is]]
            [hive-addon.composite :as comp]
            [hive-addon.protocol :as proto]))

;; SPDX-License-Identifier: MIT

;; -----------------------------------------------------------------------------
;; Fakes
;; -----------------------------------------------------------------------------

(defn- leaf
  "A well-behaved member. `opts` may carry :caps :tools :health :hooks
   :excluded :on-init, plus a :log atom recording lifecycle in order."
  [id {:keys [caps tools health hooks excluded on-init log]
       :or {caps #{} tools [] health {:status :ok} hooks {} excluded #{}}}]
  (reify proto/IAddon
    (addon-id [_] id)
    (addon-type [_] :native)
    (capabilities [_] caps)
    (initialize! [_ config]
      (when log (swap! log conj [:init id]))
      (if on-init (on-init config) {:success? true :errors []}))
    (shutdown! [_] (when log (swap! log conj [:shutdown id])) nil)
    (tools [_] tools)
    (schema-extensions [_] [])
    (health [_] health)
    (excluded-tools [_] excluded)
    (hooks [_] hooks)))

(defn- legacy
  "An addon predating the optional methods: it implements neither
   `excluded-tools` nor `hooks`, so calling them raises the host's
   unimplemented-method error."
  [id tools]
  (reify proto/IAddon
    (addon-id [_] id)
    (addon-type [_] :native)
    (capabilities [_] #{:tools})
    (initialize! [_ _] {:success? true})
    (shutdown! [_] nil)
    (tools [_] tools)
    (schema-extensions [_] [])
    (health [_] {:status :ok})))

(defn- tool [nm] {:name nm :description nm :inputSchema {:type "object"}})

(defn- started
  "Build a composite and initialize it, answering [addon init-result]."
  ([members] (started members {}))
  ([members opts]
   (let [c (comp/composite "hive.test.catalog" (assoc opts :members members))]
     [c (proto/initialize! c {})])))

;; -----------------------------------------------------------------------------
;; The outer face: a composite IS an addon
;; -----------------------------------------------------------------------------

(deftest a-composite-is-an-addon-like-any-other
  (testing "the whole point: a host mounts a catalog through the ordinary protocol"
    (let [c (comp/composite "hive.prompt" {:members [(leaf "a" {}) (leaf "b" {})]})]
      (is (proto/addon? c) "a host that cannot satisfies? this cannot mount it")
      (is (= "hive.prompt" (proto/addon-id c)))
      (is (= :native (proto/addon-type c)))
      (is (comp/composite? c))
      (is (= ["a" "b"] (comp/member-ids c))))))

(deftest a-leaf-addon-reports-no-members
  (testing "a tree walker can ask any addon for members without type-testing"
    (is (= [] (comp/member-ids (leaf "a" {}))))))

;; -----------------------------------------------------------------------------
;; Declarations vs contributions
;; -----------------------------------------------------------------------------

(deftest capabilities-span-every-member-before-anything-starts
  (testing "a host reads capabilities to ROUTE, which it does before initialize!"
    (let [c (comp/composite "cat" {:members [(leaf "a" {:caps #{:tools}})
                                             (leaf "b" {:caps #{:prompts}})]
                                   :capabilities #{:health-reporting}})]
      (is (= #{:health-reporting :tools :prompts} (proto/capabilities c))
          "declared capabilities are a static claim, not a runtime observation"))))

(deftest tools-come-only-from-members-that-actually-started
  (testing "a member that failed must not advertise a tool it cannot serve"
    (let [ok   (leaf "good" {:tools [(tool "run")]})
          bad  (leaf "bad" {:tools [(tool "run")]
                            :on-init (fn [_] {:success? false :errors ["no key"]})})
          [c res] (started [ok bad])]
      (is (= ["good_run"] (mapv :name (proto/tools c)))
          "the failed member's tool is absent, not merely deprioritized")
      (is (= ["no key"] (:errors res)))
      (is (= ["bad"] (:failed (:metadata res)))))))

;; -----------------------------------------------------------------------------
;; Tool naming
;; -----------------------------------------------------------------------------

(deftest prefixing-keeps-a-catalog-of-siblings-addressable
  (testing "three techniques all exporting `render` stay distinguishable"
    (let [[c _] (started [(leaf "hive.prompt.chain-of-thought" {:tools [(tool "render")]})
                          (leaf "hive.prompt.self-consistency" {:tools [(tool "render")]})])]
      (is (= ["chain_of_thought_render" "self_consistency_render"]
             (sort (mapv :name (proto/tools c))))))))

(deftest tool-prefix-folds-the-last-segment-only
  (testing "the host already namespaces by the composite id; repeating the stem spells it twice"
    (is (= "chain_of_thought" (comp/tool-prefix "hive.prompt.chain-of-thought")))
    (is (= "convert" (comp/tool-prefix "haystack/convert")))
    (is (= "cot" (comp/tool-prefix "COT")))))

(deftest flat-naming-reports-a-collision-rather-than-dropping-one
  (testing "silently keeping one of two identically-named tools is how a technique goes missing"
    (let [members (comp/normalize-members
                   [(leaf "a" {:tools [(tool "render")]})
                    (leaf "b" {:tools [(tool "render")]})])
          {:keys [tools collisions]} (comp/aggregate-tools members :flat nil)]
      (is (= ["render" "render"] (mapv :name tools)) "both survive; neither is hidden")
      (is (= {"render" ["a" "b"]} collisions) "and the ambiguity is named")))
  (testing "prefixing resolves it, so nothing is reported"
    (let [members (comp/normalize-members
                   [(leaf "a" {:tools [(tool "render")]})
                    (leaf "b" {:tools [(tool "render")]})])]
      (is (empty? (:collisions (comp/aggregate-tools members :prefix nil)))))))

(deftest a-flat-collision-surfaces-in-health
  (let [[c _] (started [(leaf "a" {:tools [(tool "render")]})
                        (leaf "b" {:tools [(tool "render")]})]
                       {:tool-naming :flat})]
    (is (= {"render" ["a" "b"]} (:tool-collisions (:details (proto/health c)))))))

;; -----------------------------------------------------------------------------
;; Ordering
;; -----------------------------------------------------------------------------

(deftest members-initialize-in-dependency-order
  (testing "declared sibling deps are ordered by the mount solver, not by vector position"
    (let [log (atom [])
          a   {:member/addon (leaf "a" {:log log}) :member/depends-on #{"b"}}
          b   {:member/addon (leaf "b" {:log log})}
          [_ res] (started [a b])]
      (is (= [[:init "b"] [:init "a"]] @log))
      (is (:success? res)))))

(deftest a-dependency-cycle-excludes-those-members-and-says-so
  (testing "the solver's graceful contract, surfaced as member failure"
    (let [a {:member/addon (leaf "a" {}) :member/depends-on #{"b"}}
          b {:member/addon (leaf "b" {}) :member/depends-on #{"a"}}
          c {:member/addon (leaf "c" {:tools [(tool "t")]})}
          [comp-addon res] (started [a b c])]
      (is (= #{"a" "b"} (set (:failed (:metadata res)))))
      (is (= ["c"] (:mounted (:metadata res))) "the acyclic member still mounts")
      (is (= ["c_t"] (mapv :name (proto/tools comp-addon)))))))

;; -----------------------------------------------------------------------------
;; Failure policy
;; -----------------------------------------------------------------------------

(deftest a-failing-member-degrades-the-composite-rather-than-killing-it
  (let [bad  (leaf "bad" {:on-init (fn [_] (throw (ex-info "boom" {})))})
        good (leaf "good" {:tools [(tool "t")]})
        [c res] (started [bad good])]
    (is (:success? res) "the catalog still serves what came up")
    (is (= :degraded (:status (proto/health c))))
    (is (= ["good_t"] (mapv :name (proto/tools c))))
    (is (contains? (:failed (:details (proto/health c))) "bad"))))

(deftest fail-policy-makes-any-member-failure-fatal
  (let [[c res] (started [(leaf "bad" {:on-init (fn [_] {:success? false :errors ["x"]})})
                          (leaf "good" {})]
                         {:on-member-failure :fail})]
    (is (false? (:success? res)))
    (is (= :down (:status (proto/health c))))))

(deftest a-composite-whose-every-member-failed-is-not-a-success
  (let [[_ res] (started [(leaf "a" {:on-init (fn [_] {:success? false :errors ["x"]})})])]
    (is (false? (:success? res)) "a catalog contributing nothing has not started"))
  (testing "but an EMPTY catalog is empty, not broken"
    (let [[c res] (started [])]
      (is (:success? res))
      (is (= :ok (:status (proto/health c)))))))

(deftest one-broken-member-does-not-blind-its-siblings
  (testing "a member throwing from `tools` must not propagate out of the composite"
    (let [boom (reify proto/IAddon
                 (addon-id [_] "boom")
                 (addon-type [_] :native)
                 (capabilities [_] #{})
                 (initialize! [_ _] {:success? true})
                 (shutdown! [_] nil)
                 (tools [_] (throw (ex-info "kaboom" {})))
                 (schema-extensions [_] [])
                 (health [_] {:status :ok})
                 (excluded-tools [_] #{})
                 (hooks [_] {}))
          [c _] (started [boom (leaf "good" {:tools [(tool "t")]})])]
      (is (= ["good_t"] (mapv :name (proto/tools c)))))))

;; -----------------------------------------------------------------------------
;; Legacy members
;; -----------------------------------------------------------------------------

(deftest a-member-predating-the-optional-methods-still-mounts
  (testing "hooks and excluded-tools default rather than throwing"
    (let [[c res] (started [(legacy "old" [(tool "go")])])]
      (is (:success? res))
      (is (= ["old_go"] (mapv :name (proto/tools c))))
      (is (= #{} (proto/excluded-tools c)))
      (is (= {} (proto/hooks c))))))

;; -----------------------------------------------------------------------------
;; Contribution folding
;; -----------------------------------------------------------------------------

(deftest hooks-and-exclusions-fold-with-mount-order-as-the-tie-break
  (let [[c _] (started [{:member/addon (leaf "first" {:hooks {:gx/score :from-first}
                                                      :excluded #{"read_file"}})}
                        {:member/addon (leaf "second" {:hooks {:gx/score :from-second
                                                               :cu/a :only-second}
                                                       :excluded #{"write_file"}})
                         :member/depends-on #{"first"}}])]
    (is (= :from-first (:gx/score (proto/hooks c)))
        "a hook contest resolves by mount order, the same way every time")
    (is (= :only-second (:cu/a (proto/hooks c))))
    (is (= #{"read_file" "write_file"} (proto/excluded-tools c)))))

(deftest health-folds-member-statuses
  (is (= :ok (comp/fold-health [])))
  (is (= :ok (comp/fold-health [:ok :ok])))
  (is (= :degraded (comp/fold-health [:ok :down])))
  (is (= :down (comp/fold-health [:down :down])))
  (testing "a degraded member degrades the whole"
    (let [[c _] (started [(leaf "a" {:health {:status :degraded}}) (leaf "b" {})])]
      (is (= :degraded (:status (proto/health c)))))))

;; -----------------------------------------------------------------------------
;; Lifecycle
;; -----------------------------------------------------------------------------

(deftest initialize-is-idempotent
  (let [log (atom [])
        c   (comp/composite "cat" {:members [(leaf "a" {:log log})]})]
    (is (:success? (proto/initialize! c {})))
    (is (= {:success? true :already-initialized? true} (proto/initialize! c {})))
    (is (= [[:init "a"]] @log) "the second call must not re-init the members")))

(deftest shutdown-runs-in-reverse-mount-order-and-is-idempotent
  (let [log (atom [])
        a   {:member/addon (leaf "a" {:log log}) :member/depends-on #{"b"}}
        b   {:member/addon (leaf "b" {:log log})}
        c   (comp/composite "cat" {:members [a b]})]
    (proto/initialize! c {})
    (reset! log [])
    (proto/shutdown! c)
    (is (= [[:shutdown "a"] [:shutdown "b"]] @log)
        "mounted b then a, so tear down a then b")
    (is (nil? (proto/shutdown! c)) "a second shutdown is a no-op, never a throw")
    (is (= [[:shutdown "a"] [:shutdown "b"]] @log))
    (is (empty? (proto/tools c)) "a shut-down catalog advertises nothing")))

(deftest member-config-merges-over-the-composites-own
  (let [seen (atom nil)
        m    {:member/addon (leaf "a" {:on-init (fn [cfg] (reset! seen cfg) {:success? true})})
              :member/config {:k :member-wins :extra 1}}
        c    (comp/composite "cat" {:members [m]})]
    (proto/initialize! c {:k :composite :shared true})
    (is (= {:k :member-wins :extra 1 :shared true} @seen))))

;; -----------------------------------------------------------------------------
;; Nesting — the reason the composite exists
;; -----------------------------------------------------------------------------

(deftest a-composite-is-a-legal-member-of-another-composite
  (testing "the structure is a tree, not two layers"
    (let [inner (comp/composite "hive.prompt.cot"
                                {:members [(leaf "zero-shot" {:tools [(tool "render")]
                                                              :caps #{:prompts}})
                                           (leaf "few-shot" {:tools [(tool "render")]})]})
          outer (comp/composite "hive.prompt"
                                {:members [inner (leaf "registry" {:tools [(tool "list")]})]})
          res   (proto/initialize! outer {})]
      (is (:success? res))
      (is (= ["hive.prompt.cot" "registry"] (comp/member-ids outer)))
      (is (= #{:prompts} (proto/capabilities outer))
          "a grandchild's declared capability reaches the root")
      (is (= ["cot_few_shot_render" "cot_zero_shot_render" "registry_list"]
             (sort (mapv :name (proto/tools outer))))
          "each level prefixes once, so the path through the tree is the tool name")
      (is (= :ok (:status (proto/health outer)))))))

(deftest a-failure-deep-in-the-tree-degrades-only-its-own-branch
  (let [inner (comp/composite "branch"
                              {:members [(leaf "bad" {:on-init (fn [_] {:success? false :errors ["e"]})})
                                         (leaf "ok" {:tools [(tool "t")]})]})
        outer (comp/composite "root" {:members [inner (leaf "sibling" {:tools [(tool "s")]})]})]
    (proto/initialize! outer {})
    (is (= ["branch_ok_t" "sibling_s"] (sort (mapv :name (proto/tools outer)))))
    (is (= :degraded (:status (proto/health outer)))
        "the root reports degraded because a leaf two levels down did not start")))

(deftest tearing-down-the-root-tears-down-the-whole-tree
  (let [log   (atom [])
        inner (comp/composite "branch" {:members [(leaf "deep" {:log log})]})
        outer (comp/composite "root" {:members [inner (leaf "shallow" {:log log})]})]
    (proto/initialize! outer {})
    (reset! log [])
    (proto/shutdown! outer)
    (is (= #{[:shutdown "deep"] [:shutdown "shallow"]} (set @log)))))

;; -----------------------------------------------------------------------------
;; Normalization
;; -----------------------------------------------------------------------------

(deftest members-accept-bare-instances-and-maps-alike
  (let [a (leaf "a" {})
        m (comp/normalize-members [a {:member/addon (leaf "b" {})} {:member/addon (leaf "c" {}) :member/id "renamed"}])]
    (is (= ["a" "b" "renamed"] (mapv :member/id m))
        "an explicit :member/id overrides the addon's own"))
  (testing "a malformed entry is dropped rather than throwing at construction"
    (is (= [] (comp/normalize-members [nil 42 {:member/addon "not-an-addon"}])))))
