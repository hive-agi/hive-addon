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
            [clojure.set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk])
  (:import [java.io PushbackReader]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private src-root "src")

(def roots
  "The portable ENTRY POINTS. Everything these reach, transitively, is the
   portable stratum and is bound by the rules below.

   This list is exactly the set of namespaces test/portable/oracle.cljc
   exercises, and that correspondence is the whole discipline: a namespace is
   admitted when its BEHAVIOUR has been shown identical on the JVM, cljw and
   cljrs, never when it merely loads. hive-addon.mount.solve loaded and ran on
   cljrs while returning a wrong mount order; hive-addon.plug.merge loaded on
   cljrs and threw on first call. So do not add a root here to express an
   intention — add it when the oracle covers it.

   Absent, each for a MEASURED reason (2026-08-22):

   - hive-addon.protocol and hive-addon.mount.port. Both are malli-free and load
     on all three hosts, but neither can be EXERCISED on cljrs: their whole job
     is hosting IAddon implementations, and cljrs cannot implement a protocol
     from another namespace — the same limitation that puts the IMountDriver leg
     in oracle_driver.cljc. mount.port's records were nonetheless fixed to read
     fields through `this`, so they are ready the day that lands.

   - Everything whose closure reaches malli: hive-addon.schema, mount.schema,
     plug.schema, capability, plug, mount.entitlement. The obstacle is NOT the
     Maven coordinate — laying malli's own source on the classpath does not
     help, because malli does not load on either host, and on cljrs the blocker
     is a CHAIN rather than a single defect:
       cljw  — malli.core, malli.registry and malli.impl.regex all load;
               malli.util fails to resolve `get`, which its ns form excludes
               from clojure.core and defines itself, and malli.error requires
               malli.util.
       cljrs — `(defprotocol ^:private Driver ...)` was rejected; that is now
               fixed upstream, and malli.impl.regex advances to the next link:
               `deftype` is unimplemented, and after that the namespace's hash
               sits in a `#?(:bb/:clj/:cljs)` with no `:default`, which elides
               on cljrs entirely. So this is a feature project, not a patch.

     That bounds the RUNTIME closure only. Schemas remain a JVM-side boundary
     layer, so hive-schemas coverage of these subjects is unaffected — see
     hive-addon.plug.portable-trifecta-test and
     hive-addon.mount.solve-trifecta-test."
  '#{hive-addon.hot.strategy
     hive-addon.hot.port
     hive-addon.hot.cascade
     hive-addon.plug.lint
     hive-addon.plug.merge
     hive-addon.plug.source
     hive-addon.cli
     hive-addon.cli.tree
     hive-addon.cli.response
     hive-addon.registry.tools
     hive-addon.registry.extension
     hive-addon.registry.commands
     hive-addon.registry.schema})

(def schema-roots
  "The SCHEMA TIER — portable to the JVM and cljw, but not cljrs.

   Everything here reaches malli, so it is a TWO-host claim, pinned by
   test/portable/oracle_schema.cljc rather than by portable.oracle. It is
   separated for the same reason the IMountDriver leg is: a diff that can never
   be empty teaches the reader to ignore diffs.

   cljrs is the only thing missing, and for a reason outside this repo:
   malli.impl.regex needs `deftype`, which cljrs does not implement. The
   hive-addon code itself is ready — two defects here were fixed to get cljw
   running, and both were the kind this file exists to catch:
     hive-addon.schema        matched an exception CLASS (`AbstractMethodError`)
                              inside #?(:clj ...), which cljw TAKES because it
                              presents :clj. Now detects by message.
     mount.entitlement        used a NON-TOTAL #?(:clj Throwable :cljs :default)
                              in a catch, which matches neither feature on cljrs
                              — the clause would vanish and a throwing licence
                              gate would escape instead of denying.

   These namespaces may carry reader conditionals (they cannot avoid a catch
   class), so the zero-conditionals rule does not apply to them. The TOTALITY
   rule does, and it is the one that matters."
  '#{hive-addon.schema
     hive-addon.mount.schema
     hive-addon.plug.schema
     hive-addon.capability
     hive-addon.plug
     hive-addon.mount.entitlement})

(def opaque-roots
  "The OPAQUE KERNEL TIER: the namespaces a COMPILED PROPRIETARY ADDON loads.

   A vendor writes an IAddon, `cljw build` turns it into one native binary, and
   the host mounts it through hive-addon.opaque.addon. Whatever these two roots
   reach is compiled INTO that binary, which makes this tier's rule different in
   kind from the other two: it is not about behaving identically across hosts,
   it is about being LOADABLE AT ALL under `cljw build`.

   That is why the rule below is about the require closure rather than about
   syntax. malli does not load on cljw, so a contract written beside
   `codec/encode` would not fail a style check, it would fail the vendor's
   build. The contracts therefore live in hive-addon.opaque.contracts, outside
   this tier and host-side only, and the closure test is what keeps them there.

   A JVM + cljw claim, pinned behaviourally by test/portable/oracle_opaque.cljc.
   cljrs is absent for two independent reasons: `serve` dispatches through a
   protocol from another namespace, which cljrs cannot do (the same limitation
   behind oracle_driver.cljc), and cljrs is not installed here, so any claim
   about it would be an assertion rather than a measurement.

   hive-addon.opaque.codec is written to the STRICTER three-host rules and
   satisfies all of them today (see the zero-conditional test below). It is
   deliberately NOT in `roots`: this repo admits a namespace when the oracle has
   MEASURED it on three runtimes, never to express an intention. The day cljrs
   is installed, run the oracle and move it."
  '#{hive-addon.opaque.codec
     hive-addon.opaque.serve})

(def kernel-tier-allowed-foreign
  "Namespaces OUTSIDE this tier that a kernel-tier namespace may still require.

   `clojure.*` is the host's own core, present on every runtime by definition.
   hive-addon.protocol is the IAddon protocol itself: the kernel exists to
   implement it, it is malli-free, and it is governed by the schema tier's
   rules, which are stricter than nothing.

   Everything else is refused, hive-dsl included. Not because hive-dsl would
   break, but because every namespace admitted here becomes bytes in every
   vendor's binary, and a leaf that nothing needs is a cost paid by the whole
   marketplace."
  '#{hive-addon.protocol})

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

(defn schema-tier-nses
  "Namespaces the schema tier OWNS — its closure minus the three-host stratum.

   The subtraction matters: `hive-addon.plug` requires plug.lint/merge/source,
   which are already three-host namespaces, so the raw closure overlaps. Each
   namespace must be governed by exactly one rule set, and the stricter one
   wins — a shared namespace stays a three-host claim."
  []
  (let [idx @index
        three-host (portable-nses)]
    (into (sorted-set)
          (comp (filter idx) (remove three-host))
          (closure idx schema-roots))))

(defn opaque-closure
  "The FULL require closure of the kernel tier, in-repo namespaces only."
  []
  (let [idx @index]
    (into (sorted-set) (filter idx) (closure idx opaque-roots))))

(defn opaque-tier-nses
  "Namespaces the kernel tier OWNS: its closure minus the two tiers already
   governed. Each namespace must be governed by exactly one rule set, and
   hive-addon.protocol is reached by the schema tier as well, so it stays there."
  []
  (let [three-host (portable-nses)
        schema     (schema-tier-nses)]
    (into (sorted-set)
          (comp (remove three-host) (remove schema))
          (opaque-closure))))

(defn foreign-requires
  "Every namespace the given `nses` require that is not itself among them."
  [nses]
  (let [idx @index
        own (set nses)]
    (into (sorted-set)
          (comp (mapcat (fn [n] (get-in idx [n :requires]))) (remove own))
          nses)))

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

(defn bare-record-fields
  "Every [RecordName field] where a `defrecord` method body reads a field as a
   BARE SYMBOL.

   cljrs does not bind a record's fields inside its method bodies — it answers
   `unbound symbol: sha` — while `(:sha this)` works on every host. This is the
   house OCP idiom (a protocol plus records for each open seam), so the bare
   form is both the natural thing to write and the thing that breaks.

   A method PARAMETER may legitimately share a field's name and does shadow it
   correctly, so each method is scanned for its own fields MINUS its parameters
   rather than for the record's whole field set."
  [forms]
  (let [hits (atom [])]
    (walk/postwalk
     (fn [x]
       (when (and (seq? x) (= 'defrecord (first x)) (vector? (nth x 2 nil)))
         (let [rec-name (nth x 1)
               fields   (set (nth x 2))]
           (doseq [m (drop 3 x)
                   :when (and (seq? m) (vector? (second m)))]
             (let [params  (set (filter symbol? (second m)))
                   visible (into #{} (remove params) fields)]
               (walk/postwalk
                (fn [y]
                  (when (and (symbol? y) (contains? visible y))
                    (swap! hits conj [rec-name y]))
                  y)
                (drop 2 m))))))
       x)
     forms)
    (distinct @hits)))

(defn self-shadowing-params
  "Every `defn`/`defn-` whose parameter vector contains the function's OWN name.

   On cljrs the self-reference binding wins over such a parameter, so the body
   sees the FUNCTION rather than the argument — `(defn text [text] ...)`
   returned `{:text #<Fn text>}`. A parameter named after any other var shadows
   correctly, and so does a `let`; only this collision breaks."
  [forms]
  (let [hits (atom [])]
    (walk/postwalk
     (fn [x]
       (when (and (seq? x) (contains? '#{defn defn-} (first x)) (symbol? (second x)))
         (let [fn-name (second x)]
           (doseq [param (filter vector? x)]
             (when (some #(= fn-name %) param)
               (swap! hits conj fn-name)))))
       x)
     forms)
    (distinct @hits)))

(defn non-total-reader-conditionals
  "Every reader conditional in `forms` that has NO `:default` branch.

   A total `#?` is the one shape that is safe across these hosts — `:clj`
   selects the JVM AND cljw (both have Throwable), `:default` catches cljs and
   cljrs (which presents `:rust`). A NON-total one silently ELIDES on any host
   whose feature is unlisted, and elision is the dangerous outcome: an omitted
   catch clause turns a handled refusal into a propagating exception, and an
   omitted expression turns a binding into an unbound symbol.

   Returns the clause list of each offender, so a failure names the form."
  [forms]
  (let [hits (atom [])]
    (walk/postwalk
     (fn [x]
       (when (instance? clojure.lang.ReaderConditional x)
         (let [clauses (:form x)
               features (take-nth 2 clauses)]
           (when-not (some #(= :default %) features)
             (swap! hits conj (vec clauses)))))
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

(deftest portable-stratum-reads-no-bare-record-fields
  (doseq [ns (portable-nses)]
    (testing (str ns)
      (is (empty? (bare-record-fields (get-in @index [ns :forms])))
          (str ns " reads a defrecord field as a bare symbol; cljrs does not bind"
               " record fields inside method bodies — use (:field this)")))))

(deftest portable-stratum-has-no-self-shadowing-params
  (doseq [ns (portable-nses)]
    (testing (str ns)
      (is (empty? (self-shadowing-params (get-in @index [ns :forms])))
          (str ns " has a defn whose parameter shares the function's own name;"
               " on cljrs the body sees the FUNCTION, not the argument")))))

(deftest schema-tier-obeys-the-portability-rules
  (testing "every reader conditional in the schema tier is TOTAL"
    ;; This is the rule that governs a JVM+cljw claim, and it is the one that
    ;; was actually violated: mount.entitlement's catch was
    ;; #?(:clj Throwable :cljs :default), which matches neither feature on a
    ;; host presenting :rust. An elided catch clause turns a licence-gate
    ;; refusal into a propagating exception.
    (doseq [ns (schema-tier-nses)]
      (is (empty? (non-total-reader-conditionals (get-in @index [ns :forms])))
          (str ns " has a reader conditional with no :default branch"))))

  (testing "the cljrs-specific rules are deliberately NOT applied here"
    ;; `for`, non-self-evaluating :or defaults, bare record fields and
    ;; self-shadowing params are all cljrs divergences. cljw handles every one
    ;; of them exactly as the JVM does — measured, not assumed — so enforcing
    ;; them on a tier that cannot reach cljrs anyway would be cargo cult, and
    ;; would fail today on hive-addon.plug's three `for` comprehensions for no
    ;; reachable benefit.
    ;;
    ;; This testing block is the REMINDER: if cljrs ever implements `deftype`
    ;; and this tier becomes a three-host claim, these namespaces must be moved
    ;; into `roots` and will then have to satisfy the full rule set.
    (is (seq (for-bindings (get-in @index ['hive-addon.plug :forms])))
        "hive-addon.plug still uses `for` — this assertion exists so the day it
         stops being true, someone re-reads why the rule was skipped")))

(deftest the-three-host-stratum-is-stricter-than-the-schema-tier
  (testing "zero reader conditionals in the three-host stratum, not merely total
            ones — it has no catch-class problem to solve, so the stronger rule
            costs nothing and keeps the host-interop door shut"
    (doseq [ns (portable-nses)]
      (is (zero? (reader-conditional-count (get-in @index [ns :forms]))) (str ns))))

  (testing "and every namespace is governed by exactly one rule set"
    (is (empty? (clojure.set/intersection (set (portable-nses))
                                          (set (schema-tier-nses))))
        "schema-tier-nses subtracts the three-host stratum, so a shared
         namespace stays a three-host claim rather than being governed twice")
    (is (seq (schema-tier-nses))
        "the schema tier must not be empty — an empty set would satisfy every
         rule above vacuously")))

(deftest opaque-kernel-tier-requires-nothing-a-cljw-build-cannot-load
  ;; THE admission rule for this tier, and the one that decides whether the
  ;; marketplace works at all. Everything the kernel roots reach is compiled
  ;; into every vendor's binary, and malli does not load under `cljw build`, so
  ;; a require added here is not a style regression: it is a vendor whose build
  ;; stops working, discovered by the vendor rather than by us.
  ;;
  ;; Stated as a WHITELIST over the require closure, not as a malli blacklist.
  ;; A blacklist only catches the one dependency we thought of; hive-schemas,
  ;; hive-spi and a transitive pull through hive-dsl would all pass it.
  (let [nses    (opaque-closure)
        foreign (foreign-requires nses)
        illegal (remove (fn [n]
                          (or (contains? kernel-tier-allowed-foreign n)
                              (str/starts-with? (str n) "clojure.")))
                        foreign)]
    (is (seq nses) "the closure must resolve, or this test checks nothing")
    (is (every? nses opaque-roots)
        "every kernel root must be indexed; a typo'd root would check nothing")
    (is (empty? illegal)
        (str "the opaque kernel tier requires " (pr-str (vec illegal))
             ", which is compiled into every vendor's binary. Contracts and"
             " schemas belong in hive-addon.opaque.contracts / .schema, which"
             " are HOST-side and outside this tier."))))

(deftest opaque-kernel-tier-conditionals-are-total
  ;; The JVM+cljw rule. `serve` cannot avoid a catch class, and a NON-total
  ;; conditional elides on any host whose feature is unlisted, which turns the
  ;; graceful optional-method default into a crash on the exact addon the
  ;; default exists for.
  (doseq [ns (opaque-tier-nses)]
    (testing (str ns)
      (is (empty? (non-total-reader-conditionals (get-in @index [ns :forms])))
          (str ns " has a reader conditional with no :default branch")))))

(deftest opaque-codec-is-already-three-host-clean
  ;; hive-addon.opaque.codec is held to the STRICTER rules even though it is not
  ;; yet a three-host claim, because that claim is one measurement away: cljrs is
  ;; simply not installed here. Enforcing the rules now means the day it is
  ;; installed the work is running the oracle, not repairing the namespace.
  ;;
  ;; This is also what pins `decode`'s deliberate refusal to catch. Catching
  ;; needs a catch class, a catch class needs a reader conditional, and a reader
  ;; conditional is exactly what this assertion forbids. The rescue therefore
  ;; lives in serve and addon, which are boundaries that own one anyway.
  (let [forms (get-in @index ['hive-addon.opaque.codec :forms])]
    (is (some? forms) "codec must be indexed for this test to mean anything")
    (is (zero? (reader-conditional-count forms))
        "codec carries a reader conditional; :clj does not select the JVM")
    (is (empty? (for-bindings forms))
        "codec uses `for`; cljrs errors on a second binding and SILENTLY ignores :when")
    (is (empty? (unevaluated-or-defaults forms))
        "codec has a non-self-evaluating :or default; cljrs binds it UNEVALUATED")
    (is (empty? (bare-record-fields forms))
        "codec reads a defrecord field as a bare symbol; cljrs does not bind them")
    (is (empty? (self-shadowing-params forms))
        "codec has a defn whose parameter shares its own name")))

(deftest every-namespace-is-governed-by-exactly-one-tier
  (let [three-host (set (portable-nses))
        schema     (set (schema-tier-nses))
        opaque     (set (opaque-tier-nses))]
    (is (empty? (clojure.set/intersection three-host opaque)))
    (is (empty? (clojure.set/intersection schema opaque)))
    (is (seq opaque)
        "the opaque tier must not be empty; an empty set satisfies every rule
         above vacuously")
    (testing "and the tier owns exactly the two roots, protocol having been
              claimed by the schema tier first"
      ;; Not decoration: it says WHERE hive-addon.protocol is governed. Were it
      ;; to fall out of the schema tier it would land here silently, and this
      ;; tier's rules are the weaker of the two.
      (is (= '#{hive-addon.opaque.codec hive-addon.opaque.serve} opaque))
      (is (contains? schema 'hive-addon.protocol)))))

(deftest totality-rule-is-discriminating
  (testing "non-total conditionals are flagged and total ones are not"
    (is (seq (non-total-reader-conditionals
              (read-string {:read-cond :preserve} "(try x (catch #?(:clj Throwable :cljs :default) t t))")))
        "the shape that bit mount.entitlement must be caught")
    (is (empty? (non-total-reader-conditionals
                 (read-string {:read-cond :preserve} "(try x (catch #?(:clj Throwable :default :default) t t))")))
        "the total shape must pass")
    (is (seq (non-total-reader-conditionals
              (read-string {:read-cond :preserve} "#?(:clj 1)"))))))

(deftest rules-are-discriminating
  (testing "each rule fires on code known to violate it, so a green stratum is
            evidence rather than a vacuous pass"
    (let [forms-of (fn [ns] (get-in @index [ns :forms]))]
      (testing "against real namespaces in this repo"
        (is (pos? (reader-conditional-count (forms-of 'hive-addon.schema)))
            "hive-addon.schema has #?(:clj ...) host interop")
        (is (seq (for-bindings (forms-of 'hive-addon.plug)))
            "hive-addon.plug uses `for`")
        (is (seq (unevaluated-or-defaults (forms-of 'hive-addon.mount.boundary)))
            "hive-addon.mount.boundary has symbol/fn :or defaults"))

      ;; bare-record-fields and self-shadowing-params are checked against LITERAL
      ;; forms only, and that is deliberate. A file-based discrimination check
      ;; stops discriminating the moment someone fixes the namespace it points
      ;; at — which happened here within the hour: this test named
      ;; hive-addon.mount.port/AtomMountHost as the bare-field witness, and then
      ;; AtomMountHost was fixed to read through `this`. Literal forms cannot be
      ;; fixed out from under the check.
      (testing "against literal forms, which no future cleanup can erase"
        (is (= '([R x])
               (bare-record-fields '[(defrecord R [x] IP (m [_] x))])))
        (is (empty?
             (bare-record-fields '[(defrecord R [x] IP (m [this] (:x this)))]))
            "keyword access on this is the portable form and must not be flagged")
        (is (empty?
             (bare-record-fields '[(defrecord R [x] IP (m [x] x))]))
            "a method PARAMETER may share a field name; it shadows correctly")
        (is (= '(text) (self-shadowing-params '[(defn text [text] text)])))
        (is (empty? (self-shadowing-params '[(defn text [content] content)])))
        ;; and the other three, so every rule has a witness nothing can erase
        (is (pos? (reader-conditional-count
                   (read-string {:read-cond :preserve} "(defn f [] #?(:clj 1 :cljs 2))"))))
        (is (seq (for-bindings '[(defn f [xs] (for [x xs :when (odd? x)] x))])))
        (is (seq (unevaluated-or-defaults '[(defn f [{:keys [k] :or {k SOME-VAR}}] k)])))
        (is (empty? (unevaluated-or-defaults '[(defn f [{:keys [k] :or {k false}}] k)]))
            "a self-evaluating default is fine and must not be flagged"))

      (testing "and no violating namespace is in the portable stratum"
        (let [nses (portable-nses)]
          (is (not-any? nses '[hive-addon.schema hive-addon.plug
                               hive-addon.mount.boundary hive-addon.mount.port
                               hive-addon.protocol])))))))
