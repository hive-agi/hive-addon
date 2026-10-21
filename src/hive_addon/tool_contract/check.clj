(ns hive-addon.tool-contract.check
  "CI entry point for the root-tool contract.

     clojure -M:test -m hive-addon.tool-contract.check [--require-descriptions] my.addon/->addon [more/ctor ...]

   Each argument names a zero-arg fn returning an IAddon. Its `tools` are
   checked; violations print and the process exits 1, warnings print and do
   not fail. `--require-descriptions` turns an undescribed property into a
   violation. Exit 2 when an argument cannot be resolved or called.

   `--manifests` also checks every ctor named by the repo's OWN
   resources/META-INF/hive-addons/*.edn whose :addon/capabilities has :tools,
   called as the mounter calls it, with one config map. One CI line, no
   per-repo ctor list; exit 2 if no such manifest exists."
  (:require [hive-addon.protocol :as proto]
            [hive-addon.tool-contract :as contract]
            [clojure.edn :as edn]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn addon-tools
  "Resolve `ctor-sym`, call it with `args` (default none), and return
   {:ctor sym :tools [...]} or {:ctor sym :error msg}. Manifest ctors take the
   mounter's one config argument, so `--manifests` passes [{}]."
  ([ctor-sym] (addon-tools ctor-sym []))
  ([ctor-sym args]
   (try
     (let [ctor  (requiring-resolve (symbol ctor-sym))
           addon (apply ctor args)]
       (if (proto/addon? addon)
         {:ctor ctor-sym :tools (vec (proto/tools addon))}
         {:ctor ctor-sym :error (str "constructor did not return an IAddon; got "
                                     (if (nil? addon) "nil" (.getName (class addon))))}))
     (catch Throwable e
       {:ctor ctor-sym :error (str (.getSimpleName (class e)) ": " (ex-message e))}))))

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

(def manifest-dir
  "Where a repo keeps its OWN mount manifests. Read from the working tree, not
   the classpath: the classpath also carries every dependency's manifests."
  "resources/META-INF/hive-addons")

(defn manifest-ctors
  "Pure: mount-manifest maps -> sorted, distinct ctor strings
   (\"init-ns/init-fn\") of every manifest whose :addon/capabilities has :tools."
  [manifests]
  (->> manifests
       (filter #(contains? (set (:addon/capabilities %)) :tools))
       (keep (fn [{:addon/keys [init-ns init-fn]}]
               (when (and init-ns init-fn) (str init-ns "/" init-fn))))
       distinct
       sort
       vec))

(defn read-manifests
  "Read every *.edn under `dir` (default `manifest-dir`). Returns
   {:manifests [map ...] :errors [str ...]}; a missing dir yields neither."
  ([] (read-manifests manifest-dir))
  ([dir]
   (reduce (fn [acc ^java.io.File f]
             (try (update acc :manifests conj (edn/read-string (slurp f)))
                  (catch Exception e
                    (update acc :errors conj (str (.getPath f) ": " (ex-message e))))))
           {:manifests [] :errors []}
           (->> (.listFiles (java.io.File. ^String dir))
                (filter #(.endsWith (.getName ^java.io.File %) ".edn"))
                (sort-by #(.getName ^java.io.File %))))))

(defn with-manifest-ctors
  "Plan the run: explicit ctors are called with no args, manifest ctors (under
   {:manifests? true}) with the mounter's config arg `{}`. Returns
   {:calls [[ctor-str args] ...]} or {:exit 2 :lines [...]}: an unreadable
   manifest, or a --manifests run that ends with nothing to check, is exit 2
   (a gate that silently checks nothing is worse than a red one)."
  ([parsed] (with-manifest-ctors parsed read-manifests))
  ([{:keys [opts ctors]} read-fn]
   (let [explicit (mapv #(vector % []) ctors)]
     (if-not (:manifests? opts)
       {:calls explicit}
       (let [{:keys [manifests errors]} (read-fn)
             found (->> (manifest-ctors manifests)
                        (remove (set ctors))
                        (mapv #(vector % [{}])))
             calls (into explicit found)]
         (cond
           (seq errors)   {:exit 2 :lines (mapv #(str "ERROR manifest " %) errors)}
           (empty? calls) {:exit 2 :lines [(str "no :tools manifest under " manifest-dir)]}
           :else          {:calls calls}))))))

(def ^:private flags
  {"--require-descriptions" {:require-descriptions? true}
   "--manifests"            {:manifests? true}})

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
  (let [{:keys [opts unknown] :as parsed} (parse-args args)
        resolved (when (empty? unknown) (with-manifest-ctors parsed))
        {:keys [exit lines]} (cond
                               (seq unknown)
                               {:exit 2 :lines [(str "unknown flag(s): " (pr-str unknown))]}
                               (:exit resolved)
                               resolved
                               (seq (:calls resolved))
                               (verdict (map #(apply addon-tools %) (:calls resolved))
                                        (dissoc opts :manifests?))
                               :else
                               {:exit 2 :lines ["usage: -m hive-addon.tool-contract.check [--require-descriptions] [--manifests] [ns/ctor ...]"]})]
    (doseq [l lines] (println l))
    (shutdown-agents)
    (System/exit exit)))
