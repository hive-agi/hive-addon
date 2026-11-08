(ns hive-addon.cli.tree-test
  "Facets for command-path parsing: a property that parse-command splits on
   whitespace into keywords and round-trips through `name`, and goldens over
   handler-tree resolution with mutants of the default-handler fallback."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.cli.response :as response]
            [hive-addon.cli.tree :as tree]
            [hive-addon.contracts]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private gen-segment
  (gen/fmap #(apply str %) (gen/vector gen/char-alpha 1 6)))

(defspec parse-command-splits-and-round-trips 200
  (prop/for-all [segs (gen/vector gen-segment 1 4)
                 pad  (gen/elements [" " "  " "\t"])]
    (let [cmd (str pad (str/join pad segs) pad)
          out (tree/parse-command cmd)]
      (and (vector? out)
           (every? keyword? out)
           (= segs (mapv name out))))))

(deftest blank-and-nil-parse-to-nil
  (is (nil? (tree/parse-command "   ")))
  (is (nil? (apply tree/parse-command [nil]))))

(def ^:private h1 (fn [_] :list))
(def ^:private h2 (fn [_] :default))

(def handlers
  {:status {:list h1 :_handler h2}
   :ping   h1})

(defn- resolve-view
  "resolve-handler's answer with the handler replaced by a stable tag."
  [f path]
  (let [r (f handlers path)]
    (cond-> r
      (:handler r) (update :handler #(cond (= % h1) :h1 (= % h2) :h2 :else :other))
      (:tree r)    (assoc :tree :subtree))))

(defn resolve-view* [path] (resolve-view tree/resolve-handler path))

(tri/deftrifecta resolve-handler-paths
  hive-addon.cli.tree-test/resolve-view*
  {:golden-path "test/golden/hive-addon/cli-resolve-handler.edn"
   :cases       {:leaf           [:ping]
                 :nested         [:status :list]
                 :subtree-default [:status]
                 :deeper-default [:status :nope :x]
                 :not-found      [:nope]}
   :mutations   [["no-default-fallback"
                  (fn [path] (let [r (resolve-view* path)]
                               (if (= :h2 (:handler r)) {:error :not-found :path-used [] :remaining (vec path)} r)))]
                 ["drops-remaining"
                  (fn [path] (let [r (resolve-view* path)] (cond-> r (:remaining r) (assoc :remaining []))))]]})

(deftest help-lists-every-path
  (is (= (str "Available commands:\n  - ping\n  - status\n  - status list")
         (tree/format-help handlers))))

(deftest responses-have-their-shape
  (is (= {:type "text" :text "x"} (response/text "x")))
  (is (true? (response/error? (response/error "boom"))))
  (is (false? (response/error? (response/text "fine")))))
