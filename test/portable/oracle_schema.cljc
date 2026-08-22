(ns portable.oracle-schema
  "The SCHEMA-TIER leg of the differential oracle — JVM and cljw.

   These namespaces reach malli, so they are a two-host claim rather than a
   three-host one, and they live here rather than in portable.oracle for the
   same reason the IMountDriver leg does: a permanently-red diff teaches the
   reader to ignore diffs.

   cljw runs them. That took two fixes on OUR side — hive-addon.schema matched
   an exception CLASS (`AbstractMethodError`) inside `#?(:clj ...)`, which cljw
   takes because it presents `:clj`, and hive-addon.mount.entitlement's catch
   used a NON-TOTAL `#?(:clj Throwable :cljs :default)` that matches neither
   feature on cljrs. Both are now host-free or total.

   cljrs cannot run them: malli.impl.regex needs `deftype`, which cljrs does not
   implement. That is the only thing keeping this leg off the third host — the
   hive-addon code itself is ready.

   Requires malli + dynaload SOURCE on the classpath: cljw resolves :git/url and
   :local/root coordinates only, so a Maven-only malli is simply absent there."
  (:require [hive-addon.capability :as cap]
            [hive-addon.mount.entitlement :as ent]
            [hive-addon.mount.schema :as ms]
            [hive-addon.plug.schema :as ps]
            [hive-addon.schema :as s]
            [portable.oracle :as oracle]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def spec
  {:addon/id "a" :addon/type :native :addon/init-ns "p.a" :addon/init-fn "make"})

;; --- mount schemas ---------------------------------------------------------
;; Each schema is asserted BOTH ways. A validator stuck on `true` passes every
;; positive case, so the negative line is the load-bearing one.

(oracle/emit "ms/spec-valid"    (ms/validate ms/MountSpec spec))
(oracle/emit "ms/spec-missing"  (ms/validate ms/MountSpec (dissoc spec :addon/id)))
(oracle/emit "ms/spec-badtype"  (ms/validate ms/MountSpec (assoc spec :addon/type :nope)))
(oracle/emit "ms/plan-valid"    (ms/validate ms/MountPlan {:ordered [spec] :cycles #{}
                                                           :missing {} :unmet-capabilities {}
                                                           :duplicates {}}))
(oracle/emit "ms/plan-bad"      (ms/validate ms/MountPlan {:ordered "nope"}))
(oracle/emit "ms/solve-args"    (ms/validate ms/SolveArgs [[spec]]))
(oracle/emit "ms/humanize?"     (some? (ms/humanize-errors ms/MountSpec (dissoc spec :addon/id))))

;; --- plug schemas ----------------------------------------------------------

(oracle/emit "ps/source-mvn"    (ps/validate ps/Source {:mvn/version "1.0"}))
(oracle/emit "ps/source-git"    (ps/validate ps/Source {:git/url "u" :git/sha "3bd5f82ab12cd34"}))
(oracle/emit "ps/source-bad"    (ps/validate ps/Source {:nope 1}))
(oracle/emit "ps/violations-ok" (ps/validate ps/Violations [{:rule :literal-secret :detail "x"}]))
;; the CLOSED rule enum is the point of LintRule — an unregistered id must fail
(oracle/emit "ps/violations-badrule" (ps/validate ps/Violations [{:rule :not-a-rule :detail "x"}]))
(oracle/emit "ps/violations-blank"   (ps/validate ps/Violations [{:rule :literal-secret :detail ""}]))
(oracle/emit "ps/trust-class"        (ps/validate ps/TrustClass :foss))

;; --- capability ------------------------------------------------------------

(oracle/emit "cap/declaration-status?" (some? cap/DeclarationStatus))

;; --- entitlement: a gate that throws must DENY, never escape ---------------
;; This is the one whose catch clause was non-total; on a host where the clause
;; vanished the exception would propagate instead of becoming a refusal.

(def gated-spec (assoc spec :addon/trust-class :proprietary
                            :addon/entitlement "ent-1"))

(oracle/emit "ent/ungated"      (ent/permit (fn [_] nil) spec))
(oracle/emit "ent/permitted"    (ent/permit (fn [_] nil) gated-spec))
(oracle/emit "ent/refused"      (ent/permit (fn [_] :deny/no-licence) gated-spec))
(oracle/emit "ent/throwing-gate" (ent/permit (fn [_] (throw (ex-info "boom" {}))) gated-spec))
(oracle/emit "ent/no-gate"      (ent/permit nil gated-spec))

;; --- schema/validate-addon: unimplemented detection is by MESSAGE ----------
;; The subject of the AbstractMethodError fix. A record missing a required
;; method must be a contract violation on every host, and the hosts word that
;; exception completely differently.

(defprotocol IProbe (only-method [this]))
(defrecord ProbeRec [] IProbe (only-method [_] :present))

(oracle/emit "s/validate-map-ok"  (s/validate s/AddonId "an-id"))
(oracle/emit "s/validate-map-bad" (s/validate s/AddonId 42))

(println "ORACLE-SCHEMA-END")
