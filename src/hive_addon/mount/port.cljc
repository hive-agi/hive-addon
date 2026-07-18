(ns hive-addon.mount.port
  "DIP seam for the addon mounter — the host registry abstraction.

   IMountHost is the port through which the effectful boundary registers,
   initializes, shuts down, and looks up addon instances. hive-addon ships one
   in-memory implementation (atom-mount-host) for tests, dry-run, and non-MCP
   hosts; a real host (an MCP server) supplies its own. resolve-config-default
   is the identity-ish config resolver — a host may inject a richer one (e.g.
   hive-di-backed) at the boundary.

   register!/shutdown! are no-nuke: a duplicate register! MUST NOT throw and
   shutdown! MUST NOT delete data."
  (:require [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; IMountHost — the one registry the boundary writes through (DIP)
;; =============================================================================

(defprotocol IMountHost
  "Abstraction over an addon host registry. The mounter depends on this port,
   never a concrete registry."
  (register! [host addon]
    "Put an addon instance into the host registry, keyed by its addon-id.
     Returns host. MUST NOT throw for a normal duplicate (idempotent-ish).")
  (init! [host addon-id config]
    "Initialize the registered addon with config. Returns the InitResult map
     ({:success? bool ...}).")
  (shutdown! [host addon-id]
    "Shutdown the registered addon. Returns nil. MUST NOT delete data.")
  (registered [host addon-id]
    "Fetch a mounted addon instance for sibling injection, or nil."))

;; =============================================================================
;; resolve-config-default — injected config-resolver seam
;; =============================================================================

(defn resolve-config-default
  "Default config resolver: a spec's own :addon/config (or {}). The host may
   inject a richer resolver (fn [spec] -> config-map) at the boundary."
  [spec]
  (:addon/config spec {}))

;; =============================================================================
;; atom-mount-host — in-memory IMountHost for tests / dry-run / non-MCP hosts
;; =============================================================================

(defrecord AtomMountHost [reg]
  IMountHost
  (register! [this addon]
    (swap! reg assoc (proto/addon-id addon) addon)
    this)
  (init! [_ addon-id config]
    (when-let [addon (get @reg addon-id)]
      (proto/initialize! addon config)))
  (shutdown! [_ addon-id]
    (when-let [addon (get @reg addon-id)]
      (proto/shutdown! addon))
    nil)
  (registered [_ addon-id]
    (get @reg addon-id)))

(defn atom-mount-host
  "Construct an in-memory IMountHost backed by an atom map id->addon.
   init! calls hive-addon.protocol/initialize!; shutdown! calls
   hive-addon.protocol/shutdown!."
  []
  (->AtomMountHost (atom {})))
