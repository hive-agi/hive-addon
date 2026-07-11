(ns hive-addon.plug.lint
  "Fail-closed lint of a merged iaddon config — an extensible registry of pure rules."
  (:require [hive-addon.plug.source :as src]
            [hive-dsl.result :as r]))

;; SPDX-License-Identifier: MIT

(def ^:private secret-key-re
  #"(?i)(^|[-_./])(passwd|password|pwd|pass|secret|token|api[-_]?key|private[-_]?key|client[-_]?secret|access[-_]?token|credentials?|authorization|auth)([-_./]|$)")

(def ^:private ref-cred-types #{:env :pass :config :settings-xml :git-credential})
(def ^:private cred-arity {:env 1 :pass 1 :settings-xml 1 :git-credential 1 :config 2})

(defn- secret-key? [k]
  (and (or (keyword? k) (symbol? k) (string? k))
       (boolean (re-find secret-key-re (name k)))))

(defn- literal-value? [v]
  (cond (string? v) (boolean (seq v))
        (number? v) true
        (coll? v)   (boolean (seq v))
        :else       false))

(defn- deep-secret-keys [x]
  (let [hits (transient [])]
    (letfn [(walk [y]
              (cond
                (map? y)  (doseq [[k v] y]
                            (when (and (secret-key? k) (literal-value? v)) (conj! hits k))
                            (walk v))
                (coll? y) (doseq [e y] (walk e))))]
      (walk x)
      (persistent! hits))))

(defn- smuggled-step? [step]
  (let [t (first step) args (vec (rest step))]
    (or (not (ref-cred-types t))
        (> (count args) (get cred-arity t 1))
        (some (fn [a] (and (map? a) (some #{:default :value :secret} (keys a)))) args))))

(defn- strict-class? [cls]
  (not (#{:native :foss} cls)))

(defn- rule-credentials [config]
  (for [[h cd] (:iaddon/credentials config)
        :when  (some smuggled-step? (:chain cd))]
    {:rule :literal-secret :credential h :detail "credential chain smuggles a literal value"}))

(defn- rule-plug-secrets [config]
  (for [[lib plug] (:iaddon/plugs config)
        k          (deep-secret-keys plug)]
    {:rule :literal-secret :lib lib :detail (str "inline secret key " k "; use a :credential handle")}))

(defn- rule-source-provenance [config]
  (mapcat
   (fn [[lib plug]]
     (let [coord (:source plug)
           s     (src/coord->source coord)]
       (concat
        (when (> (count (src/families coord)) 1)
          [{:rule :ambiguous-source :lib lib :detail "source declares multiple coord families"}])
        (when (and s (strict-class? (:class plug)))
          (cond
            (src/local? s)   [{:rule :source-family-mismatch :lib lib :detail ":local/root requires :class :native/:foss"}]
            (src/mutable? s) [{:rule :mutable-tag :lib lib :detail "unpinned/mutable source requires :class :native/:foss"}])))))
   (:iaddon/plugs config)))

(defn- rule-repo-userinfo [config]
  (for [[id rd] (:iaddon/repos config)
        :when   (re-find #"//[^/@\s]*:[^/@\s]*@" (str (:url rd)))]
    {:rule :literal-secret :repo id :detail "credential embedded in repo :url userinfo"}))

(def ^:private rules
  [rule-credentials rule-plug-secrets rule-source-provenance rule-repo-userinfo])

(defn check
  "Run every rule over config. (r/ok config) when clean, else
   (r/err :iaddon/lint-failed {:violations [...]})."
  [config]
  (let [violations (into [] (comp (mapcat #(% config)) (remove nil?)) rules)]
    (if (seq violations)
      (r/err :iaddon/lint-failed {:violations violations})
      (r/ok config))))
