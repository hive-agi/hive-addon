(ns hive-addon.hot.port
  "Ports through which the portable hot-reload core reaches host-specific work.

   The core — schema, cascade, strategy — is portable .cljc and names only these
   protocols. One adapter namespace per host implements them; a per-host wiring
   namespace constructs the adapters and injects them into the reload context.

   Adapters are constructed by FUNCTIONS, never held in a `def` or `defonce`: an
   instance built at load time keeps the class object of whichever protocol var
   was current then, and stops satisfying the protocol once this namespace is
   reloaded.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defprotocol ISourceResolver
  "Where an addon's constructor namespace lives, and whether its bytes can change
   under a running system."
  (-resolve-source [this ns-str]
    "Resolve `ns-str` to an AddonSource map WITHOUT :addon/id:

       {:hot/source-kind :directory | :jar | :absent
        :hot/reloadable? boolean
        :hot/source-dir  <source root, :directory only>
        :hot/source-url  <resolved location, when found>}

     MUST NOT throw. A namespace with no source resolves to :absent, which is a
     strategy input rather than an error."))

(defprotocol INsReloader
  "Namespace-level reloading."
  (-reload-nss! [this ns-strs]
    "Reload `ns-strs`. Returns {:loaded [sym] :failed sym? :error string?}."))

(defprotocol IMountDriver
  "The effectful half of a remount: shutting an addon slice down and mounting it
   again through the ordinary mount pipeline."
  (-teardown! [this host ids]
    "Shut `ids` down in reverse dependency order.
     Returns {:torn-down [id] :errors [string]}.")
  (-mount! [this plan host opts]
    "Run the mount pipeline over `plan`. Returns a MountReport."))

;; Two OPTIONAL capabilities of a mount driver, each its own role-sized port
;; (ISP). A driver that implements neither still remounts; it simply cannot
;; refuse a doomed remount before teardown, nor put the previous instances back
;; after one fails — and the RemountReport says so (:hot/down), instead of the
;; caller having to know which driver it holds.

(defprotocol IMountPreflight
  "Validate a remount BEFORE anything is torn down."
  (-preflight [this plan host opts]
    "Check that every spec in `plan` could be mounted — licence gate and
     constructor resolution — WITHOUT constructing, registering or initializing
     anything. Returns a MountReport (the dry-run shape); :ok? false refuses the
     remount while every live instance is still in place."))

(defprotocol IMountRollback
  "Remember what is running so a failed remount can put it back."
  (-snapshot [this host ids]
    "{id instance} for the `ids` that `host` currently holds, read BEFORE
     teardown. Handed to -mount! as the :fallback-instance, so an addon whose
     new instance fails is restored to its previous one, in order."))
