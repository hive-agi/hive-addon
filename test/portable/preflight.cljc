(ns portable.preflight
  "Host capability preflight for the hive-addon portable stratum.

   Emits one canonical line per CONSTRUCT, in the same `label | value` shape as
   portable.oracle, and `test/portable/run.sh` diffs each host against the JVM
   before running any oracle leg.

   Requires NOTHING from hive-addon: it probes the language, so it still answers
   when the stratum itself will not load, and a failure names the construct
   rather than a downstream behavioural difference.

   Every probe below stands for a construct the portable stratum actually uses.
   Each is either an admission rule from hive-addon.mount.solve's ns docstring or
   a divergence measured on a real host; a probe for a construct nothing uses
   would fail without telling anyone what to fix."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn emit [label v] (println (str label " | " (pr-str v))))

;; =============================================================================
;; Destructuring defaults
;; =============================================================================

(def ^:private a-default [:the :default])

(defn- via-or [{:keys [rules]}] (or rules a-default))
(defn- via-or-kw [{:keys [flag]}] (if (nil? flag) :absent flag))

;; A self-evaluating :or default is admitted; a SYMBOL default is not, which is
;; what this pair pins. cljrs bound the symbol itself, so the value arrived as
;; `a-default` rather than its vector.
(defn- via-kw-default [{:keys [n] :or {n 0}}] n)

(emit "default/or-nil"        (via-or {}))
(emit "default/or-present"    (via-or {:rules [:mine]}))
(emit "default/or-vector?"    (vector? (via-or {})))
(emit "default/kw-literal"    (via-kw-default {}))
(emit "default/kw-supplied"   (via-kw-default {:n 7}))
(emit "default/nil-flag"      (via-or-kw {}))

;; =============================================================================
;; Filtering iteration — `keep`/`mapcat` stand in for `for ... :when`
;; =============================================================================
;; A host that DROPS the `when` yields the unfiltered seq while reporting
;; success, which in a topo-sort is a wrong order and in a lint rule is a rule
;; that fires on everything.

(emit "iter/keep-when"    (vec (keep (fn [x] (when (even? x) x)) (range 8))))
(emit "iter/keep-nil"     (vec (keep (fn [_] nil) (range 4))))
(emit "iter/mapcat-keep"  (vec (mapcat (fn [x] (keep (fn [y] (when (< x y) [x y]))
                                                     (range 3)))
                                       (range 3))))
(emit "iter/into-xf-set"  (vec (sort (into #{} (map inc) [1 1 2 3]))))
(emit "iter/into-xf-comp" (vec (into [] (comp (mapcat identity) (remove nil?))
                                     [[1 nil] [nil 2] [3]])))
(emit "iter/some-contains" (boolean (some (fn [c] (contains? #{:a :b} c)) [:z :b])))
(emit "iter/some-none"     (some (fn [c] (contains? #{:a} c)) [:z]))

;; =============================================================================
;; Records — field access inside a method body
;; =============================================================================
;; cljrs does not bind a defrecord's fields as bare symbols inside its methods
;; (`unbound symbol: sha`); keyword access on `this` works on every host.

(defprotocol IProbe
  (-via-keyword [this])
  (-via-this-map [this]))

(defrecord Probe [alpha beta]
  IProbe
  (-via-keyword  [this] [(:alpha this) (:beta this)])
  (-via-this-map [this] (vec (sort (map name (keys (into {} this)))))))

(def ^:private p (->Probe "A" "B"))

(emit "record/keyword-field" (-via-keyword p))
(emit "record/keys"          (-via-this-map p))
(emit "record/assoc-extra"   (:extra (assoc p :extra :x)))
(emit "record/get-missing"   (:nope p))

;; =============================================================================
;; letfn — local recursion, as plug/merge.cljc's deep-merge uses it
;; =============================================================================

(defn- nested-merge [a b]
  (letfn [(m2 [x y]
            (if (and (map? x) (map? y))
              (reduce-kv (fn [acc k v]
                           (assoc acc k (if (contains? acc k) (m2 (get acc k) v) v)))
                         x y)
              y))]
    (m2 a b)))

(emit "letfn/recursive"  (nested-merge {:a {:b 1 :c 2}} {:a {:b 9}}))
(emit "letfn/replaces"   (nested-merge {:a {:b 1}} {:a :scalar}))
(emit "letfn/nil-right"  (nested-merge {:a 1} nil))

;; =============================================================================
;; Accumulation used by the solver
;; =============================================================================

(emit "acc/fnil-conj"   (vec (sort (get (reduce (fn [m [k v]] (update m k (fnil conj #{}) v))
                                                {}
                                                [[:x 1] [:x 2] [:y 3]])
                                        :x))))
(emit "acc/zipmap-inc"  (into (sorted-map) (reduce (fn [m k] (update m k inc))
                                                   (zipmap [:a :b] (repeat 0))
                                                   [:a :a :b])))
(emit "acc/group-count" (into (sorted-map)
                              (map (fn [[k vs]] [k (count vs)]))
                              (group-by :id [{:id "a"} {:id "a"} {:id "b"}])))
(emit "acc/juxt-into"   (into (sorted-map) (map (juxt :id :n)) [{:id "b" :n 2} {:id "a" :n 1}]))

;; =============================================================================
;; Ordering — the tie-break the solver's determinism rests on
;; =============================================================================

(emit "order/sort-strings" (vec (sort ["b10" "b2" "a" "B"])))
(emit "order/sort-kw"      (vec (sort [:z :a :m])))
(emit "order/first-of-sort" (first (sort ["delta" "alpha" "charlie"])))

;; =============================================================================
;; Transients and regex — plug/lint.cljc
;; =============================================================================

(defn- collect [xs]
  (let [hits (transient [])]
    (doseq [x xs] (when (keyword? x) (conj! hits x)))
    (persistent! hits)))

(emit "lint/transient"    (collect [:a 1 :b "c"]))
(emit "lint/re-find"      (boolean (re-find #"(?i)(^|[-_])(token|secret)([-_]|$)" "api_token")))
(emit "lint/re-find-miss" (boolean (re-find #"(?i)(^|[-_])(token|secret)([-_]|$)" "tokenize")))
(emit "lint/re-matches"   (boolean (re-matches #"[0-9a-fA-F]{7,40}" "0a1b2c3")))
(emit "lint/str-join"     (str/join "," ["a" "b"]))

;; =============================================================================
;; Errors carried as data
;; =============================================================================

(emit "ex/message" (try (throw (ex-info "boom" {:k :v}))
                        (catch #?(:clj Throwable :default :default) t (ex-message t))))
(emit "ex/data"    (try (throw (ex-info "boom" {:k :v}))
                        (catch #?(:clj Throwable :default :default) t (ex-data t))))

(println "PREFLIGHT-END")
