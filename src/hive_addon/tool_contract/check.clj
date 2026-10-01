(ns hive-addon.tool-contract.check
  "CI entry point for the root-tool contract.

     clojure -M:test -m hive-addon.tool-contract.check my.addon/->addon [more/ctor ...]

   Each argument names a zero-arg fn returning an IAddon. Its `tools` are
   checked; violations print and the process exits 1, warnings print and do
   not fail. Exit 2 when an argument cannot be resolved or called."
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
  "Pure: per-ctor results -> {:exit n :lines [str]}."
  [results]
  (let [errors  (filter :error results)
        reports (for [{:keys [ctor tools]} (remove :error results)]
                  [ctor (contract/report tools)])
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
                     reports))}))

(defn -main
  [& ctor-syms]
  (let [{:keys [exit lines]} (if (seq ctor-syms)
                               (verdict (map addon-tools ctor-syms))
                               {:exit 2 :lines ["usage: -m hive-addon.tool-contract.check ns/ctor ..."]})]
    (doseq [l lines] (println l))
    (shutdown-agents)
    (System/exit exit)))
