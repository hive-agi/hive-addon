(ns hive-addon.mount.entitlement
  "Whether a MountSpec is permitted to mount at all. Pure.

   The mounter depends on this abstraction, never on a licence implementation:
   a gate is any ILicenseGate, or any (fn [spec] -> nil | reason-keyword).
   Returning nil permits; returning a keyword refuses and names the reason.

   A spec whose :addon/trust-class is :proprietary is refused unless a gate is
   installed, so the default posture of an unconfigured host is closed."
  (:require [hive-addon.plug.schema :as ps]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defprotocol ILicenseGate
  (gate-id [this] "Keyword identifying this gate.")
  (permit? [this spec] "nil to permit, else a reason keyword."))

(def gated-trust-classes
  "Trust classes that require a gate to mount."
  #{:proprietary})

(defn gated?
  "True when `spec` may only mount through a licence gate."
  [spec]
  (contains? gated-trust-classes (:addon/trust-class spec :foss)))

(def closed-gate
  "Refuses every gated spec. The default when no gate is installed."
  (reify ILicenseGate
    (gate-id [_] :closed)
    (permit? [_ spec]
      (when (gated? spec) :deny/no-license-gate))))

(def open-gate
  "Permits everything. For development and for hosts that ship no proprietary
   addons; never appropriate where a proprietary artifact is present."
  (reify ILicenseGate
    (gate-id [_] :open)
    (permit? [_ _spec] nil)))

(defonce ^:private installed (atom closed-gate))

(defn install-gate!
  "Install `gate` as this host's licence gate. The DIP swap point: a host gains
   real licence checking by installing data-shaped behaviour, not by editing
   the mounter."
  [gate]
  (reset! installed gate)
  gate)

(defn installed-gate
  "The currently installed gate."
  []
  @installed)

(defn reset-gate!
  "Restore the closed default."
  []
  (reset! installed closed-gate))

(defn permit
  "nil when `spec` may mount under `gate`, else the refusal reason.

   `gate` is an ILicenseGate or a plain fn of the spec. A gate that throws is
   a refusal, never an escape: :deny/gate-error."
  [gate spec]
  (when (gated? spec)
    (try
      (cond
        (satisfies? ILicenseGate gate) (permit? gate spec)
        (ifn? gate) (gate spec)
        :else :deny/no-license-gate)
      ;; TOTAL reader conditional: :clj covers the JVM and cljw (both have
      ;; Throwable), :default covers cljs and cljrs. The previous
      ;; `#?(:clj Throwable :cljs :default)` matched NEITHER feature on cljrs,
      ;; so the catch clause vanished and a throwing gate would have escaped as
      ;; an exception instead of becoming :deny/gate-error — a licence gate
      ;; failing open is the one outcome this fn exists to prevent.
      (catch #?(:clj Throwable :default :default) _ :deny/gate-error))))

(def TrustClass
  "Re-exported so a MountSpec can state its trust class without reaching into
   the plug layer."
  ps/TrustClass)
