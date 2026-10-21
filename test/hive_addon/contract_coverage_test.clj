(ns hive-addon.contract-coverage-test
  "The contract gate: every public fn under the gated paths carries an m/=>
   contract (inline, or host-side in hive-addon.contracts for the malli-free
   portable stratum).

   The paths are the namespaces whose contracts are DONE. A namespace joins this
   list when its last public fn is contracted; it never joins with a blanket
   exemption. Not yet listed (cov-fleet follow-up): hive-addon.lifecycle,
   hive-addon.hot, hot.drain, hot.inject, hot.strategy, hot.source,
   hot.mount-driver, mount.boundary, mount.compose, hive-addon.mount,
   runtime.boundary, lifecycle.store, tool-contract.check, tool-contract.test,
   and the opaque tier (whose existing spine, hive-addon.opaque.contracts,
   names subjects through aliases and so registers under the alias, see the
   cov-fleet report)."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.contracts]
            [hive-schemas.coverage :as cov]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def gated-paths
  ["src/hive_addon/schema.cljc"
   "src/hive_addon/extension.cljc"
   "src/hive_addon/capability.cljc"
   "src/hive_addon/tool_contract.cljc"
   "src/hive_addon/plug.cljc"
   "src/hive_addon/plug"
   "src/hive_addon/registry"
   "src/hive_addon/cli.cljc"
   "src/hive_addon/cli"
   "src/hive_addon/composite.cljc"
   "src/hive_addon/diagnostic.clj"
   "src/hive_addon/host.clj"
   "src/hive_addon/resources.cljc"
   "src/hive_addon/terminal.cljc"
   "src/hive_addon/vessel.cljc"
   "src/hive_addon/protocol.cljc"
   "src/hive_addon/wire.cljc"
   "src/hive_addon/runtime/plan.cljc"
   "src/hive_addon/runtime/schema.cljc"
   "src/hive_addon/mount/schema.cljc"
   "src/hive_addon/mount/solve.cljc"
   "src/hive_addon/mount/port.cljc"
   "src/hive_addon/mount/injected.cljc"
   "src/hive_addon/mount/maturity.cljc"
   "src/hive_addon/mount/entitlement.cljc"
   "src/hive_addon/hot/schema.cljc"
   "src/hive_addon/hot/cascade.cljc"
   "src/hive_addon/hot/report.cljc"
   "src/hive_addon/hot/dirs.clj"
   "src/hive_addon/lifecycle/schema.cljc"
   "src/hive_addon/lifecycle/policy.cljc"
   "src/hive_addon/lifecycle/surface.cljc"
   "src/hive_addon/lifecycle/oracle.cljc"
   "src/hive_addon/lifecycle/port.cljc"
   "src/hive_addon/lifecycle/part.clj"
   "src/hive_addon/opaque/schema.cljc"])

(cov/deftest-contract-coverage every-gated-fn-carries-a-contract
  {:paths gated-paths
   :exempt {}})

(deftest the-gate-is-not-vacuous
  (let [report (cov/coverage {:paths gated-paths})]
    (is (<= 200 (count (:universe report)))
        "the universe collapsed - a gate over nothing passes for the wrong reason")
    (is (= (:universe report) (:covered report)))))
