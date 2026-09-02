(ns hive-addon.opaque
  "Facade for the OPAQUE addon seam: mount a compiled, source-free IAddon.

   The problem this solves is commercial, not technical. A proprietary addon
   cannot ship as a jar (every namespace name, every function name and the whole
   call graph read straight out of it) and cannot ship as source at all. It
   ships as ONE native binary with an embedded bytecode payload, built by
   `cljw build` from the vendor's own code written against this library's
   portable stratum, and the host mounts it through the generic proxy here.

   Nothing in the mount path is special-cased for it. An opaque addon is an
   ORDINARY mount manifest with :addon/type :external and :addon/trust-class
   :proprietary, so the existing licence gate (hive-addon.mount.entitlement)
   governs it, and an unlicensed addon's constructor namespace is never loaded.

   The vendor's side of the same wire is hive-addon.opaque.serve, and
   `entry-source` writes the entry that calls it, so the kernel and the proxy
   are framed by one library and cannot drift.

   JVM-only (.clj): it reaches the subprocess transport and writes manifest
   files. The strata below it are .cljc, and the two the KERNEL loads
   (hive-addon.opaque.codec, hive-addon.opaque.serve) are malli-free."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [hive-addon.opaque.addon :as addon]
            [hive-addon.opaque.codec :as codec]
            [hive-addon.opaque.serve :as serve]
            [hive-addon.opaque.transport.inproc :as inproc]
            [hive-addon.opaque.transport.subprocess :as subprocess]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private manifest-resource-dir "META-INF/hive-addons")

(def ^:private ctor-ns "hive-addon.opaque")

(def ^:private ctor-fn "addon-ctor")

;; =============================================================================
;; Reaching a kernel
;; =============================================================================

(defn build-argv
  "The command vector for one OpaqueSpec: the executable, then its declared
   args. A vendor whose kernel needs a licence file, a data directory or a
   feature flag passes them here, and no namespace between this one and the OS
   has to know what any of them mean."
  [spec]
  (into [(:opaque/exec spec)] (or (:opaque/args spec) [])))

(defn subprocess-addon
  "The proxy for a kernel BINARY: an OpaqueAddon over a subprocess transport.
   This is what a marketplace artifact mounts through."
  [spec]
  (addon/opaque-addon spec (subprocess/subprocess-transport (build-argv spec))))

(defn serving-transport
  "An in-process ITransport that serves a LIVE IAddon through the real wire.

   The addon is framed and answered by hive-addon.opaque.serve exactly as the
   compiled kernel would be, so this is a faithful transport rather than a stub.
   It is how a vendor proves an addon end to end BEFORE compiling it opaque, and
   how this library's own suite exercises the proxy without a binary."
  [served-addon]
  (inproc/fn-transport (fn [line] (serve/handle-line served-addon line))))

(defn inproc-addon
  "The proxy for a live IAddon over the in-process wire. Same proxy, same
   framing, no subprocess: the difference between this and `subprocess-addon` is
   exactly one transport, which is the point of the port."
  [spec served-addon]
  (addon/opaque-addon spec (serving-transport served-addon)))

(defn line-transport
  "An ITransport from a raw (request-line -> response-line) function, for a test
   that must assert the exact BYTES on the wire."
  [line-fn]
  (inproc/fn-transport line-fn))

;; =============================================================================
;; The mount seam
;; =============================================================================

(defn addon-ctor
  "The :addon/init-fn a mount manifest names. Reads the OpaqueSpec out of the
   manifest's :addon/config and answers the subprocess proxy.

   One constructor for every opaque addon in the marketplace: the manifest
   differs, the code does not."
  [config]
  (when-let [spec (:addon/config config)]
    (subprocess-addon spec)))

(defn ->manifest
  "Derive a mount manifest (plain data) from an OpaqueSpec.

   :addon/type is :external because the kernel is an out-of-process integration,
   and :addon/trust-class is :proprietary so the licence gate governs it. The
   spec rides as :addon/config, which `addon-ctor` reads back.

   :addon/capabilities carries only what must be known before the kernel runs.
   The kernel's tools are NOT enumerated here: it self-describes, so a new build
   with new tools needs no new manifest."
  [spec]
  {:addon/id           (:opaque/id spec)
   :addon/type         :external
   :addon/init-ns      ctor-ns
   :addon/init-fn      ctor-fn
   :addon/trust-class  :proprietary
   :addon/entitlement  (:opaque/id spec)
   :addon/capabilities (into #{:tools :health-reporting}
                             (or (:opaque/capabilities spec) #{}))
   :addon/description  (str "Opaque kernel sidecar: " (:opaque/id spec))
   :addon/config       spec})

(defn manifest->edn
  "Serialize a manifest to EDN text with the printer pinned, so keyword keys
   stay flat (:addon/id rather than #:addon{...}) and the bytes are a function
   of the value alone."
  [manifest]
  (binding [*print-namespace-maps* false
            *print-readably*       true
            *print-length*         nil
            *print-level*          nil]
    (with-out-str (pprint/pprint manifest))))

(defn emit!
  "Write the mount manifest for `spec` under classpath root `dir` at
   META-INF/hive-addons/<addon-id>.edn, creating parents. Returns the path.
   `dir` is supplied by the caller (a resources root), never a literal here."
  [spec dir]
  (let [manifest (->manifest spec)
        out      (io/file dir manifest-resource-dir (str (:addon/id manifest) ".edn"))]
    (io/make-parents out)
    (spit out (manifest->edn manifest))
    (.getPath out)))

;; =============================================================================
;; The vendor's side
;; =============================================================================

(defn entry-source
  "Emit the kernel entry namespace's source: a top-level call to
   hive-addon.opaque.serve/serve! over the addon `ctor-sym` builds.

   Two measured constraints are baked in and neither is obvious. A cljw `build`
   artifact receives no *command-line-args*, so the entry cannot read flags; and
   its main is never invoked, so the work must happen AT TOP LEVEL. An entry
   that defines a tidy -main compiles cleanly and then does nothing at all.

   `requires` is a vector of ns forms for the vendor's own namespaces, e.g.
   [[acme.kernel :as k]], and `ctor-sym` is a fully qualified zero-argument
   constructor returning an IAddon.

   `entry-ns` is a parameter rather than a constant because there are two build
   paths and they disagree about the name. A vendor calling `cljw build`
   directly gets the default; hive-native's opacity pipeline stages the entry as
   `kernel-main` and builds that symbol, so it passes its own. Hardcoding either
   name makes this emitter unusable from the other path, which is how a second
   copy of the entry shape gets written."
  ([requires ctor-sym] (entry-source requires ctor-sym 'kernel-entry))
  ([requires ctor-sym entry-ns]
   (str "(ns " entry-ns "\n"
        "  (:require [hive-addon.opaque.serve :as serve]"
        (when (seq requires)
          (str "\n            " (str/join "\n            " (map pr-str requires))))
        "))\n"
        "\n"
        ";; Top level on purpose: a cljw build artifact never calls an entry fn.\n"
        "(serve/serve! (" (pr-str ctor-sym) "))\n")))

;; =============================================================================
;; Re-exports, so a consumer needs one require
;; =============================================================================

(def ops
  "The closed wire vocabulary. Re-exported so a consumer that only requires this
   facade can still name an op."
  codec/ops)

(def describe
  "Project a live IAddon onto the wire, as the kernel does. Re-exported for a
   vendor checking what their addon will look like from the host side."
  serve/describe)
