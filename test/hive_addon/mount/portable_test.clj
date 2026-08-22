(ns hive-addon.mount.portable-test
  "Admission test for the PORTABLE STRATUM — the require closure of the
   hot-reload core, which must load and behave identically on the JVM, cljw and
   cljrs.

   The stratum is DERIVED, never listed: it is the transitive require closure of
   ROOTS, computed from the ns forms on disk. Adding a require to the portable
   core therefore pulls the new namespace under these rules automatically — a
   hand-maintained whitelist would silently stop covering what it names.

   Each rule below is a MEASURED divergence, not caution. The companion
   behavioural evidence is test/portable/oracle.cljc, which the three runtimes
   must answer line-for-line identically."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk])
  (:import [java.io PushbackReader]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private src-root "src")

(def roots
  "The hot-reload core. Everything these reach, transitively, is portable."
  '#{hive-addon.hot.strategy hive-addon.hot.port hive-addon.hot.cascade})

;; =============================================================================
;; Reading the stratum off disk
;; =============================================================================

(defn- read-all
  "Every top-level form in `f`, with reader conditionals PRESERVED so they can be
   detected rather than resolved away by this host's own feature set."
  [f]
  (with-open [r (PushbackReader. (io/reader f))]
    (binding [*read-eval* false]
      (let [sentinel (Object.)]
        (loop [acc []]
          (let [form (try (read {:eof sentinel :read-cond :preserve} r)
                          (catch Exception _ sentinel))]
            (if (identical? form sentinel) acc (recur (conj acc form)))))))))

(defn- ns-form [forms]
  (first (filter #(and (seq? %) (= 'ns (first %))) forms)))

(defn- required-nses [nsf]
  (->> nsf
       (filter seq?)
       (filter #(= :require (first %)))
       (mapcat rest)
       (map #(if (sequential? %) (first %) %))
       (filter symbol?)
       set))

(def ^:private index
  (delay
    (into {}
          (keep (fn [f]
                  (when (and (.isFile f) (re-find #"\.cljc?$" (.getName f)))
                    (let [forms (read-all f)]
                      (when-let [nsf (ns-form forms)]
                        [(second nsf) {:file (.getPath f)
                                       :forms forms
                                       :requires (required-nses nsf)}])))))
          (file-seq (io/file src-root)))))

(defn- closure
  "Transitive require closure of `roots` over the on-disk index."
  [idx roots]
  (loop [frontier (set roots) seen (set roots)]
    (let [nxt (into #{} (comp (mapcat #(get-in idx [% :requires])) (remove seen)) frontier)]
      (if (empty? nxt) seen (recur nxt (into seen nxt))))))

(defn portable-nses
  "The portable stratum's namespaces that live in THIS repo. Namespaces outside
   it (hive-dsl.result) are a separate portability claim, owned by their repo."
  []
  (let [idx @index]
    (into (sorted-set) (filter idx) (closure idx roots))))

;; =============================================================================
;; The three admission rules
;; =============================================================================

(defn reader-conditional-count
  "How many reader conditionals `forms` contains.

   `:clj` does NOT select the JVM — cljw presents :clj and cljrs presents :rust —
   so a #?(:clj <host interop>) branch is TAKEN on cljw and dies at analysis."
  [forms]
  (let [n (atom 0)]
    (walk/postwalk (fn [x] (when (instance? clojure.lang.ReaderConditional x) (swap! n inc)) x)
                   forms)
    @n))

(defn for-bindings
  "The binding vector of every `for` in `forms`.

   On cljrs a second binding and `:let` are unbound-symbol ERRORS, and `:when` is
   SILENTLY IGNORED — in a Kahn topo-sort that emits a wrong order while
   reporting success. mapcat/keep/reduce say the same thing on all three hosts."
  [forms]
  (let [hits (atom [])]
    (walk/postwalk
     (fn [x]
       (when (and (seq? x) (contains? '#{for clojure.core/for} (first x)))
         (swap! hits conj (second x)))
       x)
     forms)
    @hits))

(defn- self-evaluating? [x]
  (or (keyword? x) (string? x) (number? x) (boolean? x) (nil? x) (char? x)
      (and (coll? x) (not (seq? x)) (every? self-evaluating? x))))

(defn unevaluated-or-defaults
  "Every `:or` destructuring default in `forms` that is not self-evaluating.

   cljrs does not EVALUATE an :or default, so a symbol default binds the symbol
   itself and a call default binds the call's source list. Literal defaults are
   unaffected, which is exactly what hides the bug: `{:or {flag false}}` is fine
   and `{:or {rules default-rules}}` silently binds the symbol `default-rules`."
  [forms]
  (let [hits (atom [])]
    (walk/postwalk
     (fn [x]
       (when (and (map? x) (map? (:or x)))
         (doseq [[k v] (:or x)]
           (when-not (self-evaluating? v) (swap! hits conj [k v]))))
       x)
     forms)
    @hits))

;; =============================================================================
;; Tests
;; =============================================================================

(deftest stratum-is-derived-and-non-empty
  (testing "the closure resolves, so a rename cannot silently empty the stratum"
    (let [nses (portable-nses)]
      (is (seq nses))
      (is (every? (fn [r] (contains? nses r)) roots)
          "every root must itself be indexed — a typo'd root would check nothing")
      (is (contains? nses 'hive-addon.mount.solve)
          "the solver is reached through hot.cascade and must be in the closure"))))

(deftest portable-stratum-has-no-reader-conditionals
  (doseq [ns (portable-nses)]
    (testing (str ns)
      (is (zero? (reader-conditional-count (get-in @index [ns :forms])))
          (str ns " carries a reader conditional; :clj does not select the JVM")))))

(deftest portable-stratum-uses-no-for
  (doseq [ns (portable-nses)]
    (testing (str ns)
      (is (empty? (for-bindings (get-in @index [ns :forms])))
          (str ns " uses `for`; cljrs errors on a second binding/:let and SILENTLY"
               " ignores :when — use mapcat/keep/reduce")))))

(deftest portable-stratum-has-no-unevaluated-or-defaults
  (doseq [ns (portable-nses)]
    (testing (str ns)
      (is (empty? (unevaluated-or-defaults (get-in @index [ns :forms])))
          (str ns " has a non-self-evaluating :or default; cljrs binds the default"
               " UNEVALUATED — use (or x default) in the body")))))

(deftest rules-are-discriminating
  (testing "each predicate fires on a namespace known to violate it, so a green
            stratum is evidence rather than a vacuous pass"
    (let [forms-of (fn [ns] (get-in @index [ns :forms]))]
      (is (pos? (reader-conditional-count (forms-of 'hive-addon.schema)))
          "hive-addon.schema has #?(:clj ...) host interop")
      (is (seq (for-bindings (forms-of 'hive-addon.plug)))
          "hive-addon.plug uses `for`")
      (is (seq (unevaluated-or-defaults (forms-of 'hive-addon.mount.boundary)))
          "hive-addon.mount.boundary has symbol/fn :or defaults")
      (testing "and none of those three is in the portable stratum"
        (let [nses (portable-nses)]
          (is (not-any? nses '[hive-addon.schema hive-addon.plug
                               hive-addon.mount.boundary])))))))
