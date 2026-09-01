(ns hive-addon.cli.tree
  "Command-path parsing and handler-tree resolution.

   A handler tree maps keyword segments either to a handler fn or to a
   nested tree. A nested tree may carry :_handler as the value used when a
   path stops there, or when no deeper segment matches."
  (:require [clojure.string :as str]))

;; SPDX-License-Identifier: MIT

(defn normalize-command
  "COMMAND as a string. Accepts a keyword or a string; anything else is nil."
  [command]
  (cond
    (keyword? command) (name command)
    (string? command) command
    :else nil))

(defn parse-command
  "COMMAND parsed into a keyword path: \"status list\" -> [:status :list].
   Nil for a nil or blank command."
  [command]
  (when (and command (not (str/blank? command)))
    (->> (str/split (str/trim command) #"\s+")
         (mapv keyword))))

(defn collect-command-paths
  "Every callable path in HANDLERS as a seq of keyword vectors, PREFIX
   prepended. A subtree carrying :_handler also contributes its own path."
  [handlers prefix]
  (reduce-kv
   (fn [acc k v]
     (if (= k :_handler)
       acc
       (cond
         (fn? v) (conj acc (conj prefix k))
         (map? v) (let [nested (collect-command-paths v (conj prefix k))]
                    (into acc (if (contains? v :_handler)
                                (into [(conj prefix k)] nested)
                                nested)))
         :else acc)))
   [] handlers))

(defn format-help
  "Help text listing every command path in HANDLERS, one per line."
  [handlers]
  (let [paths (collect-command-paths handlers [])
        sorted (sort-by #(str/join " " (map name %)) paths)]
    (str "Available commands:\n"
         (str/join "\n" (map #(str "  - " (str/join " " (map name %))) sorted)))))

(defn- handler?
  "Is `x` a handler node — something invokable that is not itself a subtree or
   a data literal? A fn or a Var qualifies (a Var resolves through to the
   current fn at call time); a map is a subtree, and keywords/symbols/colls are
   `ifn?` without being handlers."
  [x]
  (and (ifn? x) (not (coll? x)) (not (keyword? x)) (not (symbol? x))))

(defn resolve-handler
  "Walk HANDLERS along PATH.

   Returns {:handler fn :path-used [...] :remaining [...]} on a hit,
   {:tree subtree :path-used [...]} when the path stops at a subtree with no
   :_handler, or {:error :not-found :path-used [...] :remaining [...]} when a
   segment does not match and no :_handler is available. A handler node is
   anything `handler?` admits — a fn or a Var."
  [handlers path]
  (loop [tree handlers
         used []
         remaining path]
    (if (empty? remaining)
      (if-let [h (or (when (handler? tree) tree) (get tree :_handler))]
        {:handler h :path-used used :remaining []}
        {:tree tree :path-used used})
      (let [seg (first remaining)
            next-node (get tree seg)]
        (cond
          (handler? next-node)
          {:handler next-node :path-used (conj used seg)
           :remaining (vec (rest remaining))}

          (map? next-node)
          (recur next-node (conj used seg) (rest remaining))

          :else
          (if-let [default (get tree :_handler)]
            {:handler default :path-used used :remaining (vec remaining)}
            {:error :not-found :path-used used :remaining (vec remaining)}))))))
