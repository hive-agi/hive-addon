(ns hive-addon.opaque.contracts
  "The m/=> contract spine for the opaque subsystem.

   Every contract lives HERE rather than beside its function, for one reason
   that decides the whole subsystem's shape: the two namespaces a compiled
   kernel loads (hive-addon.opaque.codec and hive-addon.opaque.serve) must be
   malli-free, because malli does not load under `cljw build`. A contract
   written next to `codec/encode` would drag the schema runtime into the
   binary. Stating them from the outside keeps the kernel lean and the host
   fully checked.

   HOST-ONLY, and loading it is OPTIONAL. Nothing requires this namespace at
   runtime; it is loaded by the suite and by a host that wants instrumentation.
   The contracts are read by hive-schemas to synthesize coverage from the same
   schemas rather than from hand-written examples.

   Schemas are referenced as VARS, not as :opaque/* registry keywords, so a
   contract resolves without anyone having installed a registry."
  (:require [hive-addon.mount.schema :as ms]
            [hive-addon.opaque :as opaque]
            [hive-addon.opaque.codec :as codec]
            [hive-addon.opaque.schema :as os]
            [hive-addon.opaque.serve :as serve]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; codec: the wire vocabulary
;; =============================================================================

(m/=> codec/op? [:=> [:cat :any] :boolean])

(m/=> codec/safe? [:=> [:cat :any] :boolean])

;; edn-safe is total: EVERY value has a projection, because the unsafe case
;; projects to nil rather than throwing. That is what makes it usable on a
;; mount config nobody has inspected.
(m/=> codec/edn-safe [:=> [:cat :any] os/WireVal])

(m/=> codec/encode [:=> [:cat :any] :string])

;; decode's argument is :any rather than [:maybe :string]: it is fed whatever
;; came off a pipe, and answering nil for a non-string is part of the contract.
(m/=> codec/decode [:=> [:cat :any] :any])

(m/=> codec/request
      [:function
       [:=> [:cat os/Op] os/Request]
       [:=> [:cat os/Op :any] os/Request]])

(m/=> codec/ok [:=> [:cat os/Op :any] os/Response])

(m/=> codec/error
      [:function
       [:=> [:cat :any] os/Response]
       [:=> [:cat :any :any] os/Response]])

(m/=> codec/error? [:=> [:cat :any] :boolean])

(m/=> codec/result [:=> [:cat :any] :any])

(m/=> codec/normalize-args [:=> [:cat :any] [:map-of :keyword :any]])

(m/=> codec/tool-summary [:=> [:cat [:map {:closed false}]] os/ToolSummary])

(m/=> codec/hook-summary [:=> [:cat :any] os/HookSummary])

(m/=> codec/health-report [:=> [:cat :boolean [:maybe [:map {:closed false}]]] :any])

(m/=> codec/init-result
      [:function
       [:=> [:cat :any] [:map {:closed false}]]
       [:=> [:cat :any [:sequential :string]] [:map {:closed false}]]])

;; =============================================================================
;; serve: the kernel side
;; =============================================================================

;; The subject is an IAddon INSTANCE, which is a protocol satisfaction rather
;; than a value shape, so it is :any here and the protocol itself is the check.
;; What is contracted is the RESULT: a kernel's self-description must conform,
;; because the proxy's whole surface is projected from it.
(m/=> serve/describe [:=> [:cat :any] os/Describe])

(m/=> serve/handle [:=> [:cat :any os/Request] os/Response])

(m/=> serve/respond [:=> [:cat :any :any] os/Response])

(m/=> serve/handle-line [:=> [:cat :any :any] :string])

;; =============================================================================
;; the facade
;; =============================================================================

(m/=> opaque/build-argv [:=> [:cat os/OpaqueSpec] [:vector :string]])

;; The manifest an opaque addon mounts through is an ORDINARY MountSpec. This
;; contract is where that claim is discharged: if the opaque seam ever needed a
;; manifest shape of its own, this line would stop validating.
(m/=> opaque/->manifest [:=> [:cat os/OpaqueSpec] ms/MountSpec])

(m/=> opaque/manifest->edn [:=> [:cat [:map {:closed false}]] :string])

(m/=> opaque/emit! [:=> [:cat os/OpaqueSpec :string] :string])

(m/=> opaque/entry-source
      [:function
       [:=> [:cat [:sequential :any] :symbol] :string]
       [:=> [:cat [:sequential :any] :symbol :symbol] :string]])
