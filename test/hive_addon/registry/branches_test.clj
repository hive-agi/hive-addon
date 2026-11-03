(ns hive-addon.registry.branches-test
  "Runtime branches through the store a host reads: a stand-in host merges
   `get-commands` over its own verbs per call, exactly as much as it needs to know."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.cli :as cli]
            [hive-addon.composite :as composite]
            [hive-addon.protocol :as proto]
            [hive-addon.registry.branches :as branches]
            [hive-addon.registry.commands :as commands]))

;; SPDX-License-Identifier: MIT

(def ^:private tool "t")

(defn- answer [s] (fn [_] {:type "text" :text s}))

(def ^:private core-tree
  {:status (answer "core status")
   :belt   {:scan (answer "core belt scan")}})

(defn- opaque [label]
  (fn [{:keys [command]}] {:type "text" :text (str label " got: " command)}))

(defn- host-call [params]
  (let [contributed (into {} (map (fn [[cmd spec]] [(keyword cmd) (:handler spec)]))
                          (commands/get-commands tool))]
    ((cli/make-handler (merge core-tree contributed)) params)))

(defn- call [command] (:text (host-call {:command command})))

(defn- routes [_] #{"status" "belt scan" "help"})

(use-fixtures :each
  (fn [t]
    (commands/clear!)
    (commands/clear-branches!)
    (commands/set-branch-host! {:tool-handler (fn [_] host-call) :routes routes})
    (try (t) (finally (commands/clear!) (commands/clear-branches!)))))

(deftest a-graft-below-a-host-subtree-keeps-its-verbs
  (is (:ok? (commands/graft! tool "belt hello" {:handler (answer "hi") :owner "x"})))
  (is (= "hi" (call "belt hello")))
  (is (= "core belt scan" (call "belt scan"))))

(deftest a-graft-below-a-host-leaf-keeps-the-leaf
  (commands/graft! tool "status extra" {:handler (answer "grafted") :owner "x"})
  (is (= "grafted" (call "status extra")))
  (is (= "core status" (call "status"))))

(deftest a-graft-below-an-opaque-contribution-leaves-its-other-routes
  (commands/contribute! tool :a {"sub" {:handler (opaque "sub")}})
  (commands/graft! tool "sub a b" {:handler (answer "grafted") :owner "x"})
  (is (= "grafted" (call "sub a b")))
  (is (= "sub got: sub a c" (call "sub a c")))
  (is (= "sub got: sub a" (call "sub a")))
  (is (= "sub got: sub" (call "sub"))))

(deftest one-addon-extends-another-and-survives-its-remount
  (commands/contribute! tool :a {"sub" {:handler {:one (answer "one")}}})
  (commands/graft! tool "sub two" {:handler (answer "two") :owner "b"})
  (commands/retract-all! :a)
  (commands/contribute! tool :a {"sub" {:handler {:one (answer "one v2")}}})
  (is (= "one v2" (call "sub one")))
  (is (= "two" (call "sub two"))))

(deftest a-graft-never-shadows-a-host-route
  (commands/graft! tool "belt scan" {:handler (answer "hijack") :owner "x"})
  (is (= "core belt scan" (call "belt scan")))
  (is (= :branch/occupied (:reason (first (commands/branch-refusals tool))))))

(deftest a-graft-never-shadows-a-declared-verb
  (commands/contribute! tool :a {"sub" {:handler (opaque "sub") :verbs ["own"]}})
  (commands/graft! tool "sub own" {:handler (answer "hijack") :owner "x"})
  (is (= "sub got: sub own" (call "sub own")))
  (is (= :branch/shadows-verb (:reason (first (commands/branch-refusals tool))))))

(deftest without-a-host-a-root-nobody-contributed-is-refused
  (commands/set-branch-host! nil)
  (commands/graft! tool "status extra" {:handler (answer "x") :owner "o"})
  (is (= "core status" (call "status")) "it must not replace a verb it cannot see")
  (is (= :branch/no-host (:reason (first (commands/branch-refusals tool))))))

(deftest around-wraps-a-host-verb
  (commands/around! tool "status" {:owner "x"
                                   :wrap (fn [n] (fn [p] (update (n p) :text str "!")))})
  (is (= "core status!" (call "status")))
  (is (= "core belt scan" (call "belt scan"))))

(deftest around-the-empty-path-wraps-every-host-root
  (commands/around! tool "" {:owner "x" :wrap (fn [n] (fn [p] (update (n p) :text str "!")))})
  (is (= "core status!" (call "status")))
  (is (= "core belt scan!" (call "belt scan"))))

(deftest lowest-order-is-outermost-whatever-arrived-first
  (commands/around! tool "status" {:owner "a" :order 10 :wrap (fn [n] (fn [p] (update (n p) :text str "-inner")))})
  (commands/around! tool "status" {:owner "b" :order 1 :wrap (fn [n] (fn [p] (update (n p) :text str "-outer")))})
  (is (= "core status-inner-outer" (call "status"))))

(deftest a-protected-path-cannot-be-wrapped
  (commands/protect! tool "script")
  (doseq [path ["" "script" "script remove"]]
    (is (= :branch/protected
           (:reason (commands/around! tool path {:owner "x" :wrap (fn [n] n)}))))))

(deftest registration-refuses-what-cannot-work
  (is (= :branch/empty-path (:reason (commands/graft! tool "" {:handler (answer "x") :owner "o"}))))
  (is (= :branch/not-invocable (:reason (commands/graft! tool "p" {:handler :kw :owner "o"}))))
  (is (= :branch/no-owner (:reason (commands/graft! tool "p" {:handler (answer "x")}))))
  (commands/graft! tool "p" {:handler (answer "first") :owner "o1"})
  (is (= :branch/owned-by (:reason (commands/graft! tool "p" {:handler (answer "x") :owner "o2"}))))
  (is (:ok? (commands/graft! tool "p" {:handler (answer "newer") :owner "o1"})))
  (is (= "newer" (call "p"))))

(deftest retract-all-takes-the-branches-too-and-notifies
  (let [seen (atom [])]
    (commands/add-listener! ::record #(swap! seen conj (:type %)))
    (try
      (commands/graft! tool "a" {:handler (answer "a") :owner :my/addon})
      (commands/retract-all! :my/addon)
      (is (empty? (commands/branches-of tool)))
      (is (= [:branch :retract] @seen))
      (finally (commands/remove-listener! ::record)))))

(deftest branches-reach-the-surface-a-host-builds-from-the-store
  (commands/graft! tool "belt hello" {:handler (answer "x") :owner "o" :summary "Greets"
                                      :params {"name" {:type "string"}}})
  (let [spec (get (commands/get-commands tool) "belt")]
    (is (= "Greets" (:summary spec)))
    (is (= {"name" {:type "string"}} (:params spec)))
    (is (contains? (:handler spec) :hello))))

(deftest no-branches-answers-the-raw-store
  (commands/contribute! tool :a {"sub" {:handler (answer "x")}})
  (is (= ["sub"] (keys (commands/get-commands tool))))
  (is (= :a (:addon (get (commands/get-commands tool) "sub")))))

(deftest composed-says-whether-a-tool-reads-the-store
  (commands/graft! tool "a" {:handler (answer "a") :owner "o"})
  (commands/forget-composition! tool)
  (is (false? (commands/composed? tool)))
  (host-call {:command "status"})
  (is (true? (commands/composed? tool))))

;; =============================================================================
;; Properties, over the pure fold
;; =============================================================================

(def ^:private gen-path (gen/vector (gen/elements [:a :b :c :d]) 1 4))

(defspec every-unrefused-graft-routes 200
  (prop/for-all [paths (gen/vector gen-path 1 6)]
    (let [gs (for [[i p] (map-indexed vector (distinct paths))]
               {:kind :graft :path p :owner (str "o" i) :handler (answer (str i))})
          {:keys [tree refusals]} (branches/fold {} gs tool {})
          refused (set (map :path refusals))
          walk    (fn [p] (let [n (get-in tree p)] (if (map? n) (:_handler n) n)))]
      (every? (fn [{:keys [path handler]}]
                (or (contains? refused path)
                    (= (:text (handler {})) (:text ((walk path) {})))))
              gs))))

;; =============================================================================
;; Live composite members
;; =============================================================================

(defrecord Member [id log fail?]
  proto/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] #{(keyword id)})
  (initialize! [_ _] (swap! log conj [:init id]) {:success? (not fail?)})
  (shutdown! [_] (swap! log conj [:stop id]) nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] {}))

(deftest a-started-composite-grows-and-shrinks
  (let [log (atom [])
        c   (composite/composite "c" {:members []})]
    (testing "not before it starts"
      (is (false? (:success? (composite/add-member! c (->Member "m" log false))))))
    (proto/initialize! c {})
    (is (:success? (composite/add-member! c (->Member "m" log false))))
    (is (= ["m"] (composite/member-ids c)))
    (is (contains? (proto/capabilities c) :m))
    (testing "the same id replaces"
      (composite/add-member! c (->Member "m" log false))
      (is (= ["m"] (composite/member-ids c)))
      (is (= [[:init "m"] [:stop "m"] [:init "m"]] @log)))
    (testing "a member that fails is recorded, not mounted"
      (is (false? (:success? (composite/add-member! c (->Member "bad" log true)))))
      (is (= ["m"] (composite/member-ids c)))
      (is (contains? (get-in (proto/health c) [:details :failed]) "bad")))
    (is (:removed? (composite/remove-member! c "m")))
    (is (empty? (composite/member-ids c)))
    (is (false? (:removed? (composite/remove-member! c "absent"))))))
