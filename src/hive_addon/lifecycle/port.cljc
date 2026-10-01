(ns hive-addon.lifecycle.port
  "Ports the lifecycle boundary drives. The host owns its registries; the
   lifecycle only asks.

   Portable stratum.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defprotocol ILifecycleHost
  (-mount! [host specs peer-specs]
    "Mount SPECS in the given order, resolving dependencies against PEER-SPECS.
     Returns a MountReport. Never throws.")
  (-unmount! [host addon-id]
    "Shut ADDON-ID down, withdraw everything it contributed, and forget the
     instance. MUST NOT delete data. Returns {:ok? bool :errors [..]}.")
  (-install-stubs! [host addon-id surface activate!]
    "Advertise SURFACE for dormant ADDON-ID. A call that reaches a stub first
     calls (activate!), which answers an ActivationReport, then routes to the
     real handler the activation contributed. Returns nil.")
  (-remove-stubs! [host addon-id]
    "Withdraw the stubs installed for ADDON-ID, leaving any real contribution
     that replaced one in place. Returns nil.")
  (-observe-surface [host addon-id]
    "The Surface mounted ADDON-ID currently shows, or nil."))

(defprotocol ISurfaceStore
  (-load-surface [store addon-id]
    "The last Surface saved for ADDON-ID, or nil.")
  (-save-surface! [store addon-id surface]
    "Remember SURFACE for ADDON-ID. Returns nil."))

(defrecord AtomSurfaceStore [a]
  ISurfaceStore
  (-load-surface [this addon-id] (get @(:a this) addon-id))
  (-save-surface! [this addon-id surface] (swap! (:a this) assoc addon-id surface) nil))

(defn atom-surface-store
  "An in-memory ISurfaceStore."
  ([] (atom-surface-store {}))
  ([initial] (->AtomSurfaceStore (atom initial))))

(defrecord NullSurfaceStore []
  ISurfaceStore
  (-load-surface [_ _] nil)
  (-save-surface! [_ _ _] nil))
