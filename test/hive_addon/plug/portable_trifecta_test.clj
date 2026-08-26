(ns hive-addon.plug.portable-trifecta-test
  "Trifecta coverage for the three plug-tier subjects admitted to the portable
   stratum, each of which was rewritten to get there:

     plug.merge/deep-merge      merge-with -> reduce-kv   (merge-with is absent on cljrs)
     plug.source/coord->source  bare field -> (:f this)   (record fields unbound on cljrs)
     plug.lint/check            for :when  -> keep        (:when silently ignored on cljrs)

   Schemas are the JVM-side boundary layer, exactly as the hot-reload port left
   them: malli is never in the runtime require closure, so it does not matter
   that malli itself does not yet load on cljw or cljrs. What these facets pin
   is the CONTRACT the three hosts must agree on, and test/portable/oracle.cljc
   is what checks they actually do.

   Every generative facet here comes in a PAIR — a schema-derived one for the
   pathological universe and a structured one that constructs the relation —
   because for all three subjects the degenerate implementation is something a
   schema-derived generator cannot distinguish:

     deep-merge      a shallow `merge` passes unless keys actually NEST and collide
     coord->source   a constant nil passes unless coords are actually well-formed
     check           a rule that never fires passes unless configs actually violate

   Each pair therefore carries a non-vacuity guard asserting the structured
   generator reaches the distinguishing case."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.plug.lint :as lint]
            [hive-addon.plug.merge :as mrg]
            [hive-addon.plug.schema :as ps]
            [hive-addon.plug.source :as src]
            [hive-schemas.test :as hst]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private real-deep-merge mrg/deep-merge)
(def ^:private real-check lint/check)

;; =============================================================================
;; deep-merge
;; =============================================================================

(defn merged-correctly?
  "Does `result` merge `a` and `b` per the documented contract — later wins per
   key, nested maps recurse, everything else replaces?

   A CHECKER, not a second implementation: it is given the answer and verifies
   it key by key, so it cannot pass by repeating deep-merge's own mistake. It is
   also the clause that separates deep from shallow — where both sides hold maps
   a shallow `merge` returns b's map outright, and the recursive call then fails
   on any key a had that b lacks."
  [a b result]
  (let [ks (into (set (keys a)) (keys b))]
    (and (map? result)
         (= ks (set (keys result)))
         (every? (fn [k]
                   (let [av (get a k ::none)
                         bv (get b k ::none)
                         rv (get result k)]
                     (cond
                       (= av ::none)            (= rv bv)
                       (= bv ::none)            (= rv av)
                       (and (map? av) (map? bv)) (merged-correctly? av bv rv)
                       ;; "others replace" — a non-map on either side wins
                       ;; outright, INCLUDING a non-map that wipes a subtree.
                       :else                    (= rv bv))))
                 ks))))

(defn folds-left?
  "Is the n-ary result the left fold of the binary case over the non-nil inputs?
   This is what makes the binary contract above cover the variadic function."
  [maps result]
  (let [ms (remove nil? maps)]
    (= result (reduce (fn [acc m] (mrg/deep-merge acc m)) nil ms))))

(def gen-nested-maps
  "Maps drawn from a SHARED, SMALL key alphabet at a shared depth, so successive
   maps genuinely collide on nested paths.

   A schema-derived generator draws keys independently and essentially never
   produces two maps whose nested paths overlap — which is exactly the case that
   separates a deep merge from a shallow `merge`. Without a shared alphabet the
   nesting property is a tautology."
  (let [ks (gen/elements [:a :b :c])]
    (gen/vector
     (gen/one-of
      [(gen/map ks gen/small-integer {:min-elements 1 :max-elements 3})
       (gen/map ks (gen/map ks gen/small-integer {:min-elements 1 :max-elements 2})
                {:min-elements 1 :max-elements 3})])
     2 4)))

(hst/deftrifecta-from-schema deep-merge-schema
  hive-addon.plug.merge/deep-merge
  {:in  ps/DeepMergeArgs
   :out [:maybe [:map-of :any :any]]
   :mutation false
   :num-tests 100
   :rel (fn [maps result]
          ;; The pathological universe: disjoint keys, nils, empty input. The
          ;; only relation that holds there is that nothing is invented and
          ;; nothing declared-once is lost.
          (let [ms (remove nil? maps)]
            (if (empty? ms)
              (nil? result)
              (and (map? result)
                   (= (set (keys result)) (into #{} (mapcat keys) ms))))))})

(defspec deep-merge-binary-matches-the-contract 200
  (prop/for-all [maps gen-nested-maps]
    (let [[a b] maps]
      (merged-correctly? a b (mrg/deep-merge a b)))))

(defspec deep-merge-folds-left-and-drops-nils 100
  (prop/for-all [maps gen-nested-maps]
    (and (folds-left? maps (apply mrg/deep-merge maps))
         ;; nil is dropped, not merged: interleaving nils cannot change the answer
         (= (apply mrg/deep-merge maps)
            (apply mrg/deep-merge (interpose nil maps))))))

(deftest deep-merge-generator-reaches-nested-collisions
  (testing "the structured generator produces inputs a SHALLOW merge gets wrong —
            otherwise the deep-merge property proves nothing"
    (let [samples (gen/sample gen-nested-maps 100)
          shallow (fn [maps] (apply merge (remove nil? maps)))
          distinguishing (count (remove (fn [maps]
                                          (= (shallow maps) (apply real-deep-merge maps)))
                                        samples))]
      (is (pos? distinguishing)
          (str "in 100 samples a shallow `merge` always agreed with deep-merge; "
               "the nesting property is vacuous"))))

  (testing "and the checker itself rejects a shallow merge, so it is not merely
            restating whatever deep-merge did"
    (let [a {:k {:x 1}} b {:k {:y 2}}]
      (is (merged-correctly? a b (real-deep-merge a b)))
      (is (not (merged-correctly? a b (merge a b)))
          "a shallow merge drops :k/:x and must fail the checker"))))

;; =============================================================================
;; coord->source
;; =============================================================================

(def gen-coord
  "A coord from each family, plus unknown coords that must classify to nil."
  (gen/one-of
   [(gen/fmap (fn [p] {:local/root p}) (gen/not-empty gen/string-alphanumeric))
    (gen/fmap (fn [u] {:git/url u :git/sha "3bd5f82ab12cd34"}) (gen/not-empty gen/string-alphanumeric))
    (gen/fmap (fn [u] {:git/url u :git/tag "v1"}) (gen/not-empty gen/string-alphanumeric))
    (gen/fmap (fn [v] {:mvn/version v}) (gen/not-empty gen/string-alphanumeric))
    (gen/return {})
    (gen/return {:unknown/key 1})]))

(hst/deftrifecta-from-schema coord-source-schema
  hive-addon.plug.source/coord->source
  {:in  ps/CoordSourceArgs
   :out :any
   :mutation false
   :num-tests 100
   ;; NB the first parameter is the ARGUMENT VECTOR, not the argument. Taking it
   ;; as the coord handed `families` a LazySeq and threw "contains? not supported
   ;; on type LazySeq" — a schema-derived facet fails loudly here, which is the
   ;; point of running one.
   :rel (fn [[coord] out]
          ;; A coord declaring no known family classifies to nil; one that does
          ;; classifies to a source whose family matches the key it declared.
          (let [fams (src/families coord)]
            (if (empty? fams)
              (nil? out)
              (contains? fams (src/family out)))))})

(defspec coord-source-family-matches-the-declared-key 200
  (prop/for-all [coord gen-coord]
    (let [out  (src/coord->source coord)
          fams (src/families coord)]
      (if (empty? fams)
        (nil? out)
        (and (some? out)
             (contains? fams (src/family out))
             ;; reading a record FIELD — the operation that was unbound on cljrs
             (boolean? (src/local? out))
             (boolean? (src/mutable? out)))))))

(deftest coord-source-generator-reaches-every-family
  (testing "all three families AND the unknown case appear, so a constant-nil or
            constant-family implementation cannot pass"
    (let [seen (into #{} (map (fn [c] (some-> (src/coord->source c) src/family)))
                     (gen/sample gen-coord 200))]
      (is (= #{:local :git :mvn nil} seen) (str "families reached: " (pr-str seen))))))

(deftest mutable-discriminates-on-the-record-field
  (testing "mutable? actually reads :sha / :version rather than answering a constant"
    (is (false? (src/mutable? (src/coord->source {:git/url "u" :git/sha "3bd5f82ab12cd34"}))))
    (is (true?  (src/mutable? (src/coord->source {:git/url "u" :git/sha "abc"}))))
    (is (true?  (src/mutable? (src/coord->source {:git/url "u" :git/tag "v1"}))))
    (is (false? (src/mutable? (src/coord->source {:mvn/version "1.0.0"}))))
    (is (true?  (src/mutable? (src/coord->source {:mvn/version "1.0-SNAPSHOT"}))))))

;; =============================================================================
;; lint/check
;; =============================================================================

(defn- violations-of [res] (get-in res [:violations] (:violations res)))

(def gen-lint-config
  "Configs that are clean or that violate exactly one rule, with the violating
   half GENERATED rather than fixed.

   `check`'s rules were `for ... :when` comprehensions. A runtime that drops
   :when reports EVERY input as violating, so a generator that only produces
   dirty configs would be satisfied by a rule that fires unconditionally. Clean
   configs are therefore half the sample and are the load-bearing half."
  (gen/let [lib (gen/elements ['org.acme/a 'org.acme/b])
            kind (gen/elements [:clean-plug :secret-plug :clean-repo :secret-repo
                                :clean-cred :secret-cred])
            word (gen/not-empty gen/string-alphanumeric)]
    (case kind
      :clean-plug  [{:iaddon/plugs {lib {:source {:mvn/version "1.0"}}}} #{}]
      :secret-plug [{:iaddon/plugs {lib {:password word}}} #{:literal-secret}]
      :clean-repo  [{:iaddon/repos {"r" {:url (str "https://" word ".com/x")}}} #{}]
      :secret-repo [{:iaddon/repos {"r" {:url (str "https://u:" word "@x.com/y")}}} #{:literal-secret}]
      :clean-cred  [{:iaddon/credentials {"h" {:chain [[:env word]]}}} #{}]
      :secret-cred [{:iaddon/credentials {"h" {:chain [[:env word {:default word}]]}}} #{:literal-secret}])))

;; No schema-derived facet for `check`. Generating IaddonConfig exhausts the heap:
;; it nests Plugs -> Plug -> Source and carries an `:any`-valued :iaddon/lock, so
;; malli's generator explores an effectively unbounded space and the run dies with
;; an OutOfMemoryError rather than a failure.
;;
;; The schema still governs the subject — `lint-fires-exactly-on-the-violating-configs`
;; below validates every result against ps/Violations on every sample, so a rule
;; reporting an unregistered id or a blank detail still fails. What is missing is
;; the pathological INPUT universe, and that is stated rather than quietly skipped.

(defspec lint-fires-exactly-on-the-violating-configs 200
  (prop/for-all [[config expected] gen-lint-config]
    (let [res   (lint/check config)
          rules (into #{} (map :rule) (violations-of res))]
      (and (= expected rules)
           (ps/validate ps/Violations (vec (violations-of res)))))))

(deftest lint-generator-produces-both-verdicts
  (testing "clean AND violating configs both appear; a rule that always fires and
            one that never fires must each be caught"
    (let [samples  (gen/sample gen-lint-config 200)
          verdicts (into #{} (map (fn [[_ expected]] (empty? expected))) samples)]
      (is (= #{true false} verdicts)
          "the generator must produce both clean and violating configs"))))

;; =============================================================================
;; Golden + mutation facets
;; =============================================================================

(def ^:private secret-config
  {:iaddon/plugs {'org.acme/a {:password "hunter2"}}
   :iaddon/repos {"r" {:url "https://u:p@example.com/x"}}
   :iaddon/credentials {"h" {:chain [[:env "X" {:default "oops"}]]}}})

(tri/deftrifecta lint-findings
  hive-addon.plug.lint/check
  {:golden-path "test/golden/hive-addon/lint-findings.edn"
   :apply?      true
   :xf          (fn [res]
                  (->> (violations-of res)
                       (map (fn [v] (select-keys v [:rule :lib :repo :credential])))
                       (sort-by (fn [v] (pr-str v)))
                       vec))
   :cases       {:clean       [{:iaddon/plugs {'org.acme/a {:source {:mvn/version "1.0"}}}}]
                 :plug-secret [{:iaddon/plugs {'org.acme/a {:password "hunter2"}}}]
                 :repo-secret [{:iaddon/repos {"r" {:url "https://u:p@example.com/x"}}}]
                 :cred-secret [{:iaddon/credentials {"h" {:chain [[:env "X" {:default "oops"}]]}}}]
                 :all-three   [secret-config]}
   :mutations
   [;; THE cljrs regression: a rule chain whose :when is ignored, so every
    ;; credential and every repo is reported. Kills the clean cases.
    ["fires-unconditionally"
     (fn always [config]
       {:violations (concat (map (fn [[lib _]] {:rule :literal-secret :lib lib})
                                 (:iaddon/plugs config))
                            (map (fn [[id _]] {:rule :literal-secret :repo id})
                                 (:iaddon/repos config))
                            (map (fn [[h _]] {:rule :literal-secret :credential h})
                                 (:iaddon/credentials config)))})]
    ;; The opposite blind spot: a rule chain that never fires. Kills every
    ;; violating case, and is what a dropped `keep` would produce.
    ["never-fires" (fn never [config] {:ok config})]
    ;; Reports only the first rule's findings — a `first` where a `mapcat`
    ;; belongs, which the :all-three case exists to catch.
    ["only-first-rule"
     (fn only-first [config]
       (let [res (real-check config)]
         (assoc res :violations (take 1 (violations-of res)))))]]})
