(ns hive-addon.opaque.transport
  "The transport PORT: a line-oriented request/response pipe to a running
   opaque kernel.

   This is the DIP swap point of the opaque subsystem. hive-addon.opaque.addon
   is one generic proxy written against this abstraction; a subprocess over a
   native binary, an in-process function, and a future in-JVM wasm host each
   implement it, and adding one is a new record rather than a change anywhere
   above (OCP).

   A transport is a PROTOCOL rather than a data profile on purpose. Transports
   differ by BEHAVIOUR (spawn, pipe, stop), and behaviour is what a protocol is
   for; the thing that differs only by CONSTANTS on this seam is the kernel spec,
   which is plain data.

   Implementors are constructed by FUNCTIONS, never held in a def: an instance
   built at load time captures the protocol object current at that moment and
   stops satisfying the protocol the next time this namespace is reloaded.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defprotocol ITransport
  "A line-oriented request/response pipe to one opaque kernel."

  (-start! [this]
    "Start the transport and return it. MUST be idempotent: the mount pipeline
     may call initialize! on an already-initialized addon.")

  (-request! [this line]
    "Send one request line and return the response line, or nil at EOF. The
     caller owns framing; a transport moves bytes and nothing else.")

  (-alive? [this]
    "True while the transport can still serve requests. MUST NOT throw: it is
     called from a health check, and a health check that throws is an outage
     report about the checker.")

  (-stop! [this]
    "Stop the transport, release its resources, and return nil. MUST be
     idempotent: shutdown! is idempotent per the IAddon contract."))
