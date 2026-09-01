(ns hive-addon.handler-ifn-test
  "A tool :handler is anything invokable — a fn OR a Var — in the schema
   exactly as in the registry's precondition, and a Var-valued CLI node is a
   handler, not a subtree."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.cli.tree :as tree]
            [hive-addon.schema :as s]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn a-handler [_params] {:type "text" :text "ok"})

(deftest a-var-is-an-acceptable-tool-handler
  (testing "the schema agrees with the registry's own precondition (ifn?)"
    (is (s/validate s/ToolDef {:name "t" :handler #'a-handler}))
    (is (s/validate s/ToolDef {:name "t" :handler a-handler}))
    (is (not (s/validate s/ToolDef {:name "t" :handler "not-invokable"})))))

(deftest a-var-valued-cli-node-resolves-as-a-handler
  (let [handlers {:status #'a-handler
                  :deep   {:_handler #'a-handler :leaf a-handler}}]
    (is (= #'a-handler (:handler (tree/resolve-handler handlers [:status]))))
    (is (= #'a-handler (:handler (tree/resolve-handler handlers [:deep]))))
    (is (= a-handler (:handler (tree/resolve-handler handlers [:deep :leaf]))))
    (testing "a subtree is still a subtree, not a handler"
      (is (map? (:tree (tree/resolve-handler {:sub {:x a-handler}} [:sub])))))))
