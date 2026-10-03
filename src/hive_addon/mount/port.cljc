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
;; IMountUnregister — the plug-out capability (ISP: a SEPARATE, optional port)
;; =============================================================================

(defprotocol IMountUnregister
  "Optional capability of an IMountHost: forget an addon instance entirely.

   Kept OUT of IMountHost on purpose (ISP). Every existing host keeps loading
   and keeps working without it; a host that does not implement it simply
   cannot plug an addon OUT — its registry keeps the shut-down entry, and the
   boundary reports that as :unsupported rather than pretending the addon is
   gone (see hive-addon.mount.boundary/unregister!).

   The documented default for a host WITHOUT this protocol: after shutdown!
   the instance stays registered (inert), `registered` may still answer it,
   and a later register! of the same id must replace it."
  (unregister! [host addon-id]
    "Drop ADDON-ID from the host registry. Called only AFTER shutdown!, so it
     MUST NOT shut the addon down a second time, and MUST NOT delete data.
     Idempotent: an unknown id is a no-op. Returns host."))

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
  ;; `(:reg this)` throughout rather than the bare field symbol `reg`: cljrs
  ;; does not bind a defrecord's fields inside its method bodies (it answers
  ;; `unbound symbol: reg`), while keyword access on `this` works on every host.
  (register! [this addon]
    (swap! (:reg this) assoc (proto/addon-id addon) addon)
    this)
  (init! [this addon-id config]
    (when-let [addon (get @(:reg this) addon-id)]
      (proto/initialize! addon config)))
  (shutdown! [this addon-id]
    (when-let [addon (get @(:reg this) addon-id)]
      (proto/shutdown! addon))
    nil)
  (registered [this addon-id]
    (get @(:reg this) addon-id))

  IMountUnregister
  (unregister! [this addon-id]
    (swap! (:reg this) dissoc addon-id)
    this))

(defn atom-mount-host
  "Construct an in-memory IMountHost backed by an atom map id->addon.
   init! calls hive-addon.protocol/initialize!; shutdown! calls
   hive-addon.protocol/shutdown!; it also implements IMountUnregister, so an
   addon can be plugged out of it."
  []
  (->AtomMountHost (atom {})))
