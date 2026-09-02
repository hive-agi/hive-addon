(ns hive-addon.vessel
  "IVessel: the contract for a host environment that provides headed
   capabilities.

   A vessel abstracts the headed environment (Emacs, tmux, VS Code, a web UI)
   behind a protocol, the way a window manager abstracts a screen. It supplies
   terminals, editors, delivery channels, REPLs, and it owns the mapping from
   an agent to the context that agent runs in.

   It lives here, beside IAddon, for the reason IAddon does: a vessel ships as
   an addon, so it must compile against the contract alone. Defining it in a
   host would force every vessel to compile-depend on that host, which is what
   this leaf lib exists to prevent.

   Vessels are a registry, not a singleton: several can be active at once
   (Emacs and tmux, Emacs and a web UI). The registry itself belongs to the
   host; only the contract is here.

   IVessel and IAddon are implemented on SEPARATE objects. Both declare
   `capabilities`, `initialize!` and `shutdown!`, so one reify cannot carry
   both.

   Reload-safety: `defprotocol` is not idempotent, so the declaration is
   guarded: re-evaluating this namespace will not orphan existing
   implementations."
  (:require [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; IVessel Protocol
;; =============================================================================

(defonce ^:private -ivessel-defined? (atom false))

(when (compare-and-set! -ivessel-defined? false true)
  (defprotocol IVessel
    "A host environment providing headed capabilities."

    (vessel-id [this]
      "The keyword identifying this vessel, used as the registry's dispatch
       key. Examples: :emacs, :tmux, :web-ui, :vscode. Must be stable.")

    (capabilities [this]
      "The set of capabilities this vessel provides, drawn from
       #{:terminal :editor :grid :delivery :repl}.")

    (resolve-context [this agent-id]
      "The context AGENT-ID runs in, as {:project-id :cwd :session-id}, or nil
       when this vessel does not know the agent. The vessel owns this mapping;
       a host must not fall back to ambient current-directory state.")

    (addon [this capability]
      "The concrete implementation of CAPABILITY, or nil when this vessel does
       not provide it.
       :terminal -> hive-addon.terminal/ITerminalAddon
       :editor   -> the host's editor contract
       :delivery -> the host's delivery-channel contract")

    (initialize! [this config]
      "Initialize the vessel from CONFIG. Called during host startup.")

    (shutdown! [this]
      "Shut the vessel down and release its resources.")))

;; =============================================================================
;; Predicates
;; =============================================================================

(defn vessel?
  "True iff X satisfies IVessel."
  [x]
  (satisfies? IVessel x))

(defn vessel-addon?
  "True iff X satisfies both IAddon and IVessel. Rare: a vessel and its addon
   are normally separate objects, because the two protocols share method
   names."
  [x]
  (and (satisfies? proto/IAddon x)
       (satisfies? IVessel x)))
