(ns hive-addon.cli-test
  "Every addon tool call arrives as a command string, so a dispatcher that
   resolves the wrong handler or swallows an unknown command routes a user's
   request into silence."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.cli :as cli]
            [hive-addon.cli.response :as response]
            [hive-addon.cli.tree :as tree]))

;; =============================================================================
;; Parsing and normalisation
;; =============================================================================

(deftest normalize-accepts-keywords-and-strings-test
  (is (= "status" (tree/normalize-command :status)))
  (is (= "status list" (tree/normalize-command "status list")))
  (is (nil? (tree/normalize-command 42)))
  (is (nil? (tree/normalize-command nil))))

(deftest parse-splits-on-whitespace-test
  (is (= [:status :list] (tree/parse-command "status list")))
  (is (= [:spawn] (tree/parse-command "spawn")))
  (is (= [:a :b] (tree/parse-command "  a   b  ")) "extra whitespace collapses")
  (is (nil? (tree/parse-command "")))
  (is (nil? (tree/parse-command "   ")))
  (is (nil? (tree/parse-command nil))))

;; =============================================================================
;; Tree resolution
;; =============================================================================

(def ^:private handlers
  {:spawn  (fn [_] (response/text "spawned"))
   :status {:_handler (fn [_] (response/text "status-default"))
            :list     (fn [_] (response/text "status-list"))}
   :deep   {:a {:b (fn [_] (response/text "deep-ab"))}}})

(deftest a-leaf-handler-resolves-test
  (let [{:keys [handler path-used remaining]} (tree/resolve-handler handlers [:spawn])]
    (is (fn? handler))
    (is (= [:spawn] path-used))
    (is (= [] remaining))))

(deftest a-subtree-default-handles-its-own-path-test
  (is (fn? (:handler (tree/resolve-handler handlers [:status])))))

(deftest an-unmatched-segment-falls-back-to-the-subtree-default-test
  (let [{:keys [handler remaining]} (tree/resolve-handler handlers [:status :nope])]
    (is (fn? handler) ":_handler catches the unknown subcommand")
    (is (= [:nope] remaining) "and the unconsumed segment is reported")))

(deftest an-unmatched-segment-without-a-default-is-not-found-test
  (let [{:keys [error handler]} (tree/resolve-handler handlers [:deep :a :nope])]
    (is (= :not-found error))
    (is (nil? handler))))

(deftest n-depth-paths-resolve-test
  (is (fn? (:handler (tree/resolve-handler handlers [:deep :a :b])))))

(deftest stopping-at-a-subtree-without-a-default-returns-the-tree-test
  (let [{:keys [tree handler]} (tree/resolve-handler handlers [:deep :a])]
    (is (nil? handler))
    (is (map? tree))))

;; =============================================================================
;; Help
;; =============================================================================

(deftest help-lists-every-path-including-subtree-defaults-test
  (let [help (tree/format-help handlers)]
    (is (re-find #"deep a b" help))
    (is (re-find #"status list" help))
    (is (re-find #"- status\n" (str help "\n"))
        "a subtree with :_handler lists its own path too")
    (is (not (re-find #"_handler" help)) ":_handler is never shown as a command")))

;; =============================================================================
;; Dispatch
;; =============================================================================

(deftest dispatch-reaches-the-right-handler-test
  (let [h (cli/make-handler handlers)]
    (is (= "spawned" (:text (h {:command "spawn"}))))
    (is (= "status-list" (:text (h {:command "status list"}))))
    (is (= "status-default" (:text (h {:command "status"}))))
    (is (= "deep-ab" (:text (h {:command "deep a b"}))))))

(deftest a-keyword-command-dispatches-too-test
  (is (= "spawned" (:text ((cli/make-handler handlers) {:command :spawn})))))

(deftest help-renders-without-a-handler-test
  (let [resp ((cli/make-handler handlers) {:command "help"})]
    (is (false? (response/error? resp)))
    (is (re-find #"Available commands" (:text resp)))))

(deftest an-unknown-or-missing-command-is-an-error-test
  (let [h (cli/make-handler handlers)]
    (is (response/error? (h {:command "nonexistent"})))
    (is (response/error? (h {:command nil})))
    (is (response/error? (h {})))
    (is (re-find #"Unknown command" (:text (h {:command "nonexistent"}))))))

(deftest params-reach-the-handler-test
  (let [h (cli/make-handler {:echo (fn [params] (response/text (:value params)))})]
    (is (= "hi" (:text (h {:command "echo" :value "hi"}))))))

(deftest a-host-may-inject-its-own-error-builder-test
  (testing "the error shape is the host's to enrich"
    (let [h (cli/make-handler handlers
                              {:error-fn (fn [m] {:type "text" :text m
                                                  :isError true :hint "try help"})})]
      (is (= "try help" (:hint (h {:command "nope"})))))))

;; =============================================================================
;; Response shape
;; =============================================================================

(deftest response-helpers-carry-the-tool-result-shape-test
  (is (= {:type "text" :text "ok"} (response/text "ok")))
  (is (= {:type "text" :text "bad" :isError true} (response/error "bad")))
  (is (false? (response/error? (response/text "ok"))))
  (is (true? (response/error? (response/error "bad")))))
