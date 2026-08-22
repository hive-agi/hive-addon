(ns portable.oracle
  "Differential oracle for the hive-addon PORTABLE STRATUM.

   Emits one canonical line per observation. The JVM, cljw and cljrs must agree
   LINE FOR LINE; `test/portable/run.sh` runs all three and diffs them. Where
   hive-addon.mount.portable-test checks the stratum's SOURCE against its
   admission rules, this checks its BEHAVIOUR — the rules exist to keep this
   diff empty, and a rule with no behavioural consequence would be superstition.

   Two sections, both required to pass:
     - the hot-reload core: solve, cascade, strategy selection, the refusal path
     - the plug/cli tier:   deep-merge, source classification, lint rules,
                            command parsing, response shapes

   Everything emitted here is canonicalized: sets are sorted, records are
   reduced to their strategy id, and no whole map containing namespaced keys is
   printed raw. Set iteration order, record print-names (`#hive_addon...` vs
   `#hive-addon...` vs `#RemountStrategy`) and the `#:addon{...}` namespaced-map
   shorthand are host-dependent and are NOT behaviour — leaving them raw
   produces diffs that say nothing, which trains the reader to ignore the diff.

   The IMountDriver leg lives in oracle_driver.cljc because cljrs cannot yet
   implement a protocol from another namespace; see that file."
  (:require [hive-addon.cli :as cli]
            [hive-addon.registry.commands :as rcmd]
            [hive-addon.registry.extension :as rext]
            [hive-addon.registry.schema :as rsch]
            [hive-addon.registry.tools :as rtool]
            [hive-addon.cli.response :as resp]
            [hive-addon.cli.tree :as tree]
            [hive-addon.hot.cascade :as cascade]
            [hive-addon.hot.strategy :as strat]
            [hive-addon.mount.solve :as solve]
            [hive-addon.plug.lint :as lint]
            [hive-addon.plug.merge :as mrg]
            [hive-addon.plug.source :as src]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn emit [label v] (println (str label " | " (pr-str v))))

(defn sorted-vals
  "Canonicalize {k #{...}} to {k [sorted...]} — set order is host-dependent."
  [m]
  (into {} (map (fn [[k v]] [k (vec (sort v))])) m))

(def specs
  "Deliberately NOT in dependency order, so :ordered is evidence about the solve
   rather than about the input's order."
  [{:addon/id "c" :addon/dependencies #{"b"}}
   {:addon/id "a"}
   {:addon/id "b" :addon/dependencies #{"a"}}
   {:addon/id "solo"}
   {:addon/id "cap-consumer" :addon/requires-capabilities #{:store}}
   {:addon/id "cap-provider" :addon/capabilities #{:store :extra}}])

(def cyclic
  [{:addon/id "x" :addon/dependencies #{"y"}}
   {:addon/id "y" :addon/dependencies #{"x"}}
   {:addon/id "free"}])

;; =============================================================================
;; solve
;; =============================================================================

(emit "edges"         (vec (sort (solve/edges specs))))
(emit "ordered"       (mapv :addon/id (:ordered (solve/solve specs))))
(emit "ordered/shuffled-input-stable"
      (= (mapv :addon/id (:ordered (solve/solve specs)))
         (mapv :addon/id (:ordered (solve/solve (reverse specs))))))
(emit "cycles"        (vec (sort (:cycles (solve/solve cyclic)))))
(emit "cycle/ordered" (mapv :addon/id (:ordered (solve/solve cyclic))))
(emit "missing"       (sorted-vals (:missing (solve/solve [{:addon/id "m" :addon/dependencies #{"gone"}}]))))
(emit "unmet-caps"    (sorted-vals (:unmet-capabilities (solve/solve [{:addon/id "u" :addon/requires-capabilities #{:nope}}]))))
;; two DISTINCT specs sharing an id: `solve` sets the spec collection first, so
;; two identical maps would collapse and the observation would be vacuous.
(emit "duplicates"    (:duplicates (solve/solve [{:addon/id "d" :addon/init-ns "one"}
                                                 {:addon/id "d" :addon/init-ns "two"}])))

;; =============================================================================
;; cascade
;; =============================================================================

(emit "dependents/a"    (vec (sort (cascade/dependents specs #{"a"}))))
(emit "dependents/b"    (vec (sort (cascade/dependents specs #{"b"}))))
(emit "dependents/solo" (vec (sort (cascade/dependents specs #{"solo"}))))
(emit "dependents/cap"  (vec (sort (cascade/dependents specs #{"cap-provider"}))))
(emit "affected-plan"   (mapv :addon/id (:ordered (cascade/affected-plan specs #{"a"} {}))))
(emit "ns->addon-ids"   (sorted-vals (cascade/ns->addon-ids [{:addon/id "p" :addon/init-ns "shared.ns"}
                                                             {:addon/id "q" :addon/init-ns "shared.ns"}])))
(emit "seeds-for-ns"    (vec (sort (cascade/seeds-for-ns [{:addon/id "p" :addon/init-ns "shared.ns"}] "shared.ns"))))

;; =============================================================================
;; strategy selection
;; =============================================================================

(emit "chain"           (mapv strat/-strategy-id (strat/installed-strategies)))
(emit "select/default"  (strat/-strategy-id (:ok (strat/select {:addon/id "a"} {}))))
(emit "select/declared" (strat/-strategy-id (:ok (strat/select {:addon/id "a" :addon/reload-strategy :inert} {}))))
(emit "select/unknown"  (:error (strat/select {:addon/id "a" :addon/reload-strategy :nope} {})))
(emit "select/jar-source"
      (strat/-strategy-id (:ok (strat/select {:addon/id "a"}
                                             {:hot/source {:hot/reloadable? false
                                                           :hot/source-kind :jar}}))))

;; =============================================================================
;; the refusal path — no driver, so nothing may be torn down
;; =============================================================================

(let [rep (strat/reload! {:addon/id "a"} {:hot/specs specs :hot/seeds #{"a"}})]
  (emit "refusal/ok?"      (:ok? rep))
  (emit "refusal/affected" (:hot/affected rep))
  (emit "refusal/errors"   (:errors rep)))


;; =============================================================================
;; plug / cli tier
;; =============================================================================

(defn- rules-of
  "Violation rule keywords from a lint result, sorted."
  [res]
  (vec (sort (map (fn [v] (:rule v)) (:violations res)))))

(emit "deep-merge/flat"    (mrg/deep-merge {:a 1} {:b 2}))
(emit "deep-merge/nested"  (mrg/deep-merge {:a {:x 1 :y 2}} {:a {:y 9 :z 3}}))
(emit "deep-merge/replace" (mrg/deep-merge {:a {:x 1}} {:a 5}))
(emit "deep-merge/deep3"   (mrg/deep-merge {:a {:b {:c 1}}} {:a {:b {:d 2}}}))
(emit "deep-merge/nils"    (mrg/deep-merge nil {:a 1} nil {:b 2}))
(emit "deep-merge/order"   (mrg/deep-merge {:a 1} {:a 2} {:a 3}))

;; reads a defrecord FIELD through `this` — the bare field symbol is unbound on cljrs
(emit "src/families-mvn"   (vec (sort (map str (src/families {:mvn/version "1.0"})))))
(emit "src/local?"         (boolean (src/local? (src/coord->source {:local/root "/x"}))))
(emit "src/mutable-branch" (boolean (src/mutable? (src/coord->source {:git/url "u" :git/branch "main"}))))
(emit "src/mutable-shortsha" (boolean (src/mutable? (src/coord->source {:git/url "u" :git/sha "abc"}))))
(emit "src/mutable-fullsha"  (boolean (src/mutable? (src/coord->source {:git/url "u" :git/sha "3bd5f82ab12cd34"}))))
(emit "src/mutable-snapshot" (boolean (src/mutable? (src/coord->source {:mvn/version "1.0-SNAPSHOT"}))))

;; each lint rule was a `for ... :when`; a runtime dropping :when fires on EVERYTHING,
;; so both the violating and the clean case are load-bearing observations
(emit "lint/clean"          (rules-of (lint/check {:iaddon/plugs {"a" {:version "1"}}})))
(emit "lint/plug-secret"    (rules-of (lint/check {:iaddon/plugs {"a" {:password "hunter2"}}})))
(emit "lint/repo-userinfo"  (rules-of (lint/check {:iaddon/repos {"r" {:url "https://u:p@example.com/x"}}})))
(emit "lint/repo-clean"     (rules-of (lint/check {:iaddon/repos {"r" {:url "https://example.com/x"}}})))
(emit "lint/cred-smuggle"   (rules-of (lint/check {:iaddon/credentials {"h" {:chain [[:env "X" {:default "oops"}]]}}})))
(emit "lint/cred-clean"     (rules-of (lint/check {:iaddon/credentials {"h" {:chain [[:env "X"]]}}})))

(emit "tree/parse"         (tree/parse-command "a b"))
(emit "tree/normalize"     (tree/normalize-command :foo))
;; the param name collided with the fn's own name; cljrs returned the FUNCTION here
(emit "resp/text"          (resp/text "hi"))
(emit "resp/error"         (resp/error "bad"))
(emit "resp/error?"        [(resp/error? (resp/error "bad")) (resp/error? (resp/text "hi"))])


;; =============================================================================
;; cli dispatch
;; =============================================================================
;; The catch below is a TOTAL reader conditional (:clj plus :default), which is
;; the one shape that is safe across these hosts: cljw matches :clj and has
;; Throwable, cljrs falls through to :default. A non-total #? would vanish on
;; cljrs and silently stop catching.

(def handlers
  {:greet (fn [params] (resp/text (str "hi " (:who params))))
   :nested {:deep (fn [_] (resp/text "deep"))
            :_handler (fn [_] (resp/text "tree-default"))}})

(def handler (cli/make-handler handlers))

(emit "cli/dispatch"        (handler {:command "greet" :who "pedro"}))
(emit "cli/keyword-command" (handler {:command :greet :who "x"}))
(emit "cli/nested"          (handler {:command "nested deep"}))
(emit "cli/tree-default"    (handler {:command "nested"}))
(emit "cli/unknown"         (:isError (handler {:command "nope"})))
(emit "cli/missing-command" (:isError (handler {})))
(emit "cli/help-is-text"    (= "text" (:type (handler {:command "help"}))))

;; an injected coercion fn is applied before the handler sees the params
(def coercing
  (cli/make-handler {:n (fn [params] (resp/text (pr-str (:n params))))}
                    {:coerce-schema {:n [:int]}
                     :coerce-fn (fn [_schema params]
                                  {:ok (update params :n (fn [v] (if (string? v) (parse-long v) v)))})}))
(emit "cli/coerced"  (coercing {:command "n" :n "42"}))

;; and declaring a schema with no coercion fn REFUSES at wiring time rather than
;; handing the handler the raw strings it asked to have coerced
(emit "cli/refuses-schema-without-fn"
      (try (do (cli/make-handler handlers {:coerce-schema {:n [:int]}}) :no-refusal)
           (catch #?(:clj Throwable :default :default) t
             (:cli/error (ex-data t)))))


;; =============================================================================
;; host-surface registries
;; =============================================================================
;; Stateful (atom-backed) and process-global, so every observation clears first
;; and reads back a value it just wrote. Order within this section is
;; significant; that is the point of clearing.

(rtool/clear!)
(rtool/register! {:name "t1" :handler (fn [_] :one)})
(rtool/register! {:name "t2" :handler (fn [_] :two)})
(emit "reg.tools/names"      (vec (sort (rtool/registered-names))))
(emit "reg.tools/get"        (some? (rtool/get-tool "t1")))
(emit "reg.tools/handler"    ((:handler (rtool/get-tool "t2")) {}))
(rtool/deregister! "t1")
(emit "reg.tools/after-drop" (vec (sort (rtool/registered-names))))
(rtool/clear!)
(emit "reg.tools/cleared"    (vec (rtool/registered-names)))

(rext/clear!)
(rext/register! :ext/a :value-a)
(rext/register-many! {:ext/b :value-b :ext/c :value-c})
(emit "reg.ext/keys"       (vec (sort (rext/registered-keys))))
(emit "reg.ext/get"        (rext/get-extension :ext/b))
(emit "reg.ext/available"  [(rext/extension-available? :ext/a) (rext/extension-available? :ext/zz)])
(rext/deregister! :ext/a)
(emit "reg.ext/after-drop" (vec (sort (rext/registered-keys))))
(rext/clear!)

(rcmd/clear!)
;; two addons contribute to the SAME tool, so retract! must remove one addon's
;; commands and leave the other's — a retraction keyed by tool alone would pass
;; a single-contributor fixture
(emit "reg.cmd/contributed-a" (vec (sort (map str (rcmd/contribute! "code" "addon.a" {:cider {:handler (fn [_] :cider) :description "cider"}})))))
(emit "reg.cmd/contributed-b" (vec (sort (map str (rcmd/contribute! "code" "addon.b" {:carto {:handler (fn [_] :carto) :description "carto"}})))))
(emit "reg.cmd/tool-names" (vec (sort (rcmd/contributed-tool-names))))
(emit "reg.cmd/commands"   (vec (sort (map str (keys (rcmd/get-commands "code"))))))
(rcmd/retract! "code" "addon.a")
(emit "reg.cmd/after-drop" (vec (sort (map str (keys (rcmd/get-commands "code"))))))
(emit "reg.cmd/survivor-addon" (:addon (get (rcmd/get-commands "code") "carto")))
(rcmd/clear!)
(emit "reg.cmd/cleared"    (vec (rcmd/contributed-tool-names)))

(rsch/clear!)
(rsch/register! "code" {"extra-param" {:type "string"}})
(emit "reg.schema/tool-names" (vec (sort (rsch/extended-tool-names))))
(emit "reg.schema/extensions" (rsch/get-extensions "code"))
(rsch/clear!)
(emit "reg.schema/cleared"    (vec (rsch/extended-tool-names)))

(println "ORACLE-END")
