(ns hive-addon.tool-contract.check
  "CI entry point for the root-tool contract.

     clojure -M:test -m hive-addon.tool-contract.check [--require-descriptions] my.addon/->addon [more/ctor ...]

   Each argument names a zero-arg fn returning an IAddon. Its `tools` are
   checked; violations print and the process exits 1, warnings print and do
   not fail. `--require-descriptions` turns an undescribed property into a
   violation. Exit 2 when an argument cannot be resolved or called."
  (:require [hive-addon.protocol :as proto]
            [hive-addon.tool-contract :as contract]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn addon-tools
  "Resolve `ctor-sym`, call it, and return {:ctor sym :tools [...]} or
   {:ctor sym :error msg}."
  [ctor-sym]
  (try
    (let [ctor (requiring-resolve (symbol ctor-sym))]
      {:ctor ctor-sym :tools (vec (proto/tools (ctor)))})
    (catch Throwable e
      {:ctor ctor-sym :error (str (.getSimpleName (class e)) ": " (ex-message e))})))

(defn verdict
  "Pure: per-ctor results -> {:exit n :lines [str]}. `opts` is passed to
   `contract/report` (e.g. {:require-descriptions? true})."
  ([results] (verdict results {}))
  ([results opts]
   (let [errors  (filter :error results)
         reports (for [{:keys [ctor tools]} (remove :error results)]
                   [ctor (contract/report tools opts)])
         bad?    (some (comp seq :violations second) reports)]
     {:exit  (cond (seq errors) 2 bad? 1 :else 0)
      :lines (concat
              (for [{:keys [ctor error]} errors]
                (str "ERROR " ctor ": " error))
              (mapcat (fn [[ctor {:keys [checked violations warnings]}]]
                        (cond-> [(str (if (seq violations) "FAIL " "ok   ") ctor
                                      " — " checked " tool(s)")]
                          (seq violations) (conj (contract/format-findings violations))
                          (seq warnings)   (conj (str "  warnings:\n"
                                                      (contract/format-findings warnings)))))
                      reports))})))

(def ^:private flags
  {"--require-descriptions" {:require-descriptions? true}})

(defn parse-args
  "Split CLI args into {:opts {...} :ctors [sym-str ...] :unknown [flag ...]}."
  [args]
  (reduce (fn [acc a]
            (cond
              (contains? flags a)          (update acc :opts merge (flags a))
              (.startsWith ^String a "--") (update acc :unknown conj a)
              :else                        (update acc :ctors conj a)))
          {:opts {} :ctors [] :unknown []}
          args))

(defn -main
  [& args]
  (let [{:keys [opts ctors unknown]} (parse-args args)
        {:keys [exit lines]} (cond
                               (seq unknown)
                               {:exit 2 :lines [(str "unknown flag(s): " (pr-str unknown))]}
                               (seq ctors)
                               (verdict (map addon-tools ctors) opts)
                               :else
                               {:exit 2 :lines ["usage: -m hive-addon.tool-contract.check [--require-descriptions] ns/ctor ..."]})]
    (doseq [l lines] (println l))
    (shutdown-agents)
    (System/exit exit)))
