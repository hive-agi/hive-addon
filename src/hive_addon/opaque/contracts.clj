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

(m/=> hive-addon.opaque.codec/op? [:=> [:cat :any] :boolean])

(m/=> hive-addon.opaque.codec/safe? [:=> [:cat :any] :boolean])

;; edn-safe is total: EVERY value has a projection, because the unsafe case
;; projects to nil rather than throwing. That is what makes it usable on a
;; mount config nobody has inspected.
(m/=> hive-addon.opaque.codec/edn-safe [:=> [:cat :any] os/WireVal])

(m/=> hive-addon.opaque.codec/encode [:=> [:cat :any] :string])

;; decode's argument is :any rather than [:maybe :string]: it is fed whatever
;; came off a pipe, and answering nil for a non-string is part of the contract.
(m/=> hive-addon.opaque.codec/decode [:=> [:cat :any] :any])

(m/=> hive-addon.opaque.codec/request
      [:function
       [:=> [:cat os/Op] os/Request]
       [:=> [:cat os/Op :any] os/Request]])

(m/=> hive-addon.opaque.codec/ok [:=> [:cat os/Op :any] os/Response])

(m/=> hive-addon.opaque.codec/error
      [:function
       [:=> [:cat :any] os/Response]
       [:=> [:cat :any :any] os/Response]])

(m/=> hive-addon.opaque.codec/error? [:=> [:cat :any] :boolean])

(m/=> hive-addon.opaque.codec/result [:=> [:cat :any] :any])

(m/=> hive-addon.opaque.codec/normalize-args [:=> [:cat :any] [:map-of :keyword :any]])

(m/=> hive-addon.opaque.codec/tool-summary [:=> [:cat [:map {:closed false}]] os/ToolSummary])

(m/=> hive-addon.opaque.codec/hook-summary [:=> [:cat :any] os/HookSummary])

(m/=> hive-addon.opaque.codec/health-report [:=> [:cat :boolean [:maybe [:map {:closed false}]]] :any])

(m/=> hive-addon.opaque.codec/init-result
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
(m/=> hive-addon.opaque.serve/describe [:=> [:cat :any] os/Describe])

;; handle's request is any decoded MAP, not os/Request: `respond` hands it
;; every map a pipe produced, and the `case` default answering "unknown op"
;; for an op outside the closed vocabulary is part of the contract (a host
;; built against a newer proxy). os/Request here made that default throw
;; under instrumentation (wire-test the-kernel-answers-garbage-instead-of-dying).
(m/=> hive-addon.opaque.serve/handle [:=> [:cat :any [:map-of :any :any]] os/Response])

(m/=> hive-addon.opaque.serve/respond [:=> [:cat :any :any] os/Response])

(m/=> hive-addon.opaque.serve/handle-line [:=> [:cat :any :any] :string])

;; =============================================================================
;; the facade
;; =============================================================================

(m/=> hive-addon.opaque/build-argv [:=> [:cat os/OpaqueSpec] [:vector :string]])

(m/=> hive-addon.opaque/deadlines
      [:=> [:cat os/OpaqueSpec]
       [:map {:closed true}
        [:request-timeout-ms [:maybe os/TimeoutMs]]
        [:init-timeout-ms [:maybe os/TimeoutMs]]]])

;; The manifest an opaque addon mounts through is an ORDINARY MountSpec. This
;; contract is where that claim is discharged: if the opaque seam ever needed a
;; manifest shape of its own, this line would stop validating.
(m/=> hive-addon.opaque/->manifest [:=> [:cat os/OpaqueSpec] ms/MountSpec])

(m/=> hive-addon.opaque/manifest->edn [:=> [:cat [:map {:closed false}]] :string])

(m/=> hive-addon.opaque/emit! [:=> [:cat os/OpaqueSpec :string] :string])

(m/=> hive-addon.opaque/entry-source
      [:function
       [:=> [:cat [:sequential :any] :symbol] :string]
       [:=> [:cat [:sequential :any] :symbol :symbol] :string]])

