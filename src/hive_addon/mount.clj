(ns hive-addon.mount
  "Facade for the addon mounter — the public surface a host consumes.

   Re-exports the pure solver (solve), the effectful boundary (mount!, dry-run,
   teardown!, discover-specs, parse-spec), the DIP config resolver + host
   constructor (resolve-config-default, atom-mount-host), and the schema
   validators/keys. mount-classpath! is the one-call composition root:
   discover-specs -> solve -> mount!. The IMountHost protocol is NOT re-exported
   (a protocol cannot be plain-def aliased) — implement it from its canonical
   home hive-addon.mount.port.

   Rationale lives in hive memory (KG-linked), not here."
  (:require [hive-addon.mount.boundary :as boundary]
            [hive-addon.mount.compose :as compose]
            [hive-addon.mount.port :as port]
            [hive-addon.mount.schema :as schema]
            [hive-addon.mount.solve :as solve]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Re-exports — pure solver
;; =============================================================================

(def solve
  "hive-addon.mount.solve/solve — pure spec-set -> MountPlan."
  solve/solve)

(def edges
  "hive-addon.mount.solve/edges — folded dependency-rule edge set."
  solve/edges)

(def default-rules
  "hive-addon.mount.solve/default-rules — the built-in ordering-rule chain."
  solve/default-rules)

;; =============================================================================
;; Re-exports — effectful boundary
;; =============================================================================

(def discover-specs
  "hive-addon.mount.boundary/discover-specs — classpath manifest scan."
  boundary/discover-specs)

(def parse-spec
  "hive-addon.mount.boundary/parse-spec — EDN string -> MountSpec Result."
  boundary/parse-spec)

(def mount!
  "hive-addon.mount.boundary/mount! — drive a plan through a host (graceful)."
  boundary/mount!)

(def dry-run
  "hive-addon.mount.boundary/dry-run — effect-free MountReport (parity)."
  boundary/dry-run)

(def teardown!
  "hive-addon.mount.boundary/teardown! — reverse-order shutdown, no-nuke."
  boundary/teardown!)

(def default-init-retry
  "hive-addon.mount.boundary/default-init-retry — one-attempt compatibility policy."
  boundary/default-init-retry)

;; =============================================================================
;; Re-exports — DIP port
;; =============================================================================

(def atom-mount-host
  "hive-addon.mount.port/atom-mount-host — in-memory IMountHost constructor."
  port/atom-mount-host)

(def resolve-config-default
  "hive-addon.mount.port/resolve-config-default — default config resolver."
  port/resolve-config-default)

;; =============================================================================
;; Re-exports — schema validators
;; =============================================================================

(def validate   "hive-addon.mount.schema/validate."   schema/validate)
(def validate*  "hive-addon.mount.schema/validate*."  schema/validate*)
(def explain    "hive-addon.mount.schema/explain."    schema/explain)

;; =============================================================================
;; Composition root
;; =============================================================================

(defn mount-classpath!
  "One-call composition root: discover-specs -> solve -> mount!. Returns a
   MountReport; discovery :errors are attached under the report's :discovery-errors
   key. opts thread to solve (:rules, :fail-closed-cycles) and mount!
   (:resolve-config, :init-retry, :on-event, :sleep-fn).

   When :fail-closed-cycles true and the discovered specs contain a cycle, solve
   returns (r/err :mount/unsolvable ...) and that error is returned as-is."
  ([host] (mount-classpath! host {}))
  ([host opts]
   (let [{:keys [specs errors]} (discover-specs)
         plan (solve specs opts)]
     (if (and (map? plan) (contains? plan :error))
       plan
       (cond-> (mount! plan host opts)
         (seq errors) (assoc :discovery-errors errors))))))

;; =============================================================================
;; Re-exports — composition root (plug-select + mount over discovered specs)
;; =============================================================================

(def compose-plan
  "hive-addon.mount.compose/compose-plan — pure select+solve over plug layers."
  compose/compose-plan)

(def compose!
  "hive-addon.mount.compose/compose! — select+order+mount! into a host."
  compose/compose!)

(def read-layers
  "hive-addon.mount.compose/read-layers — paths -> validated plug layers."
  compose/read-layers)

(def compose-classpath!
  "hive-addon.mount.compose/compose-classpath! — discover-specs + read-layers + compose!."
  compose/compose-classpath!)
