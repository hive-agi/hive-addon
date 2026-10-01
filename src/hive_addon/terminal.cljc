(ns hive-addon.terminal
  "ITerminalAddon: the contract for addon-contributed terminal backends.

   A companion protocol to IAddon. A concrete terminal backend (a vessel)
   implements BOTH on the same reify: IAddon carries lifecycle, this carries
   the terminal operations.

   It lives here, beside IAddon, for the same reason IAddon does. A vessel is
   an addon like any other, so it must be able to compile against the contract
   alone. Defining it in a host would force every vessel to compile-depend on
   that host, which is precisely what this leaf lib exists to prevent.

   Method signatures mirror a host's ling-strategy protocol exactly, same
   arities and argument semantics, so a host can dispatch to an addon
   backend through a thin adapter.

   Lifecycle: the backend is started during IAddon/initialize! and torn down
   during IAddon/shutdown!."
  (:require [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; ITerminalAddon Protocol
;; =============================================================================

(defprotocol ITerminalAddon
  "Protocol for addon-contributed terminal backends.

   Implementors must also satisfy IAddon for addon lifecycle integration."

  (terminal-id [this]
    "Return the keyword identifier for this terminal backend.
     Examples: :vterm, :opencode, :crush, :kitty, :tmux
     Must be stable: it is used as the dispatch key for backend selection
     and for mode resolution at spawn time.")

  (terminal-spawn! [this ctx opts]
    "Spawn a terminal/process using this backend's mechanism.
     Arguments:
       ctx  - Ling context map {:keys [id cwd presets project-id]}
       opts - Spawn options map {:keys [task kanban-task-id terminal presets]}
     Returns: String slave-id on success.
     Throws: ex-info on spawn failure with {:id str :error str}")

  (terminal-dispatch! [this ctx task-opts]
    "Dispatch a task to a running terminal.
     Arguments:
       ctx       - Ling context map {:keys [id]}
       task-opts - Task options {:keys [task timeout-ms] :or {timeout-ms 60000}}
     Returns: true on successful dispatch.
     Throws: ex-info on dispatch failure with {:ling-id str :error str}")

  (terminal-status [this ctx ds-status]
    "Get terminal-specific liveness and status information.
     Arguments:
       ctx       - Ling context map {:keys [id]}
       ds-status - Host status map, may be nil or {:slave/status kw}
     Returns: Status map {:slave/id str :slave/status kw} or nil.
     May include backend-specific keys (e.g. :elisp-alive? for vterm).")

  (terminal-kill! [this ctx]
    "Terminate the terminal/process using this backend's mechanism.
     Arguments: ctx - Ling context map {:keys [id]}
     Returns: {:killed? true :id str} or {:killed? false :id str :reason kw}")

  (terminal-interrupt! [this ctx]
    "Interrupt the current query/task of a running terminal.
     Arguments: ctx - Ling context map {:keys [id]}
     Returns: {:success? true :ling-id str}
              or {:success? false :ling-id str :errors [str]}"))

;; =============================================================================
;; Predicates
;; =============================================================================

(defn terminal-addon?
  "Check if object implements both IAddon and ITerminalAddon."
  [x]
  (and (satisfies? proto/IAddon x)
       (satisfies? ITerminalAddon x)))
