(ns hive-addon.opaque.serve
  "KERNEL side of the opaque wire: turn any IAddon into a line server.

   A proprietary addon is written against this library's PORTABLE stratum and
   compiled by `cljw build` into one native binary. That binary's whole job is
   `(serve! my-addon)`: read a request line, dispatch it to the addon through
   IAddon, write a response line. The host mounts hive-addon.opaque.addon on the
   other end, which speaks the same lines. Neither side knows anything about the
   other beyond hive-addon.opaque.codec.

   REQUIRES ONLY protocol + codec, both malli-free, so the compiled kernel
   carries no schema runtime and no validation code. The schemas describing
   these shapes live host-side (hive-addon.opaque.schema).

   The kernel SELF-DESCRIBES. `describe` projects the addon's whole pure
   surface, so a mount manifest never enumerates the kernel's tools and cannot
   fall out of date with the binary it names.

   A JVM+cljw claim, not a three-host one: this namespace dispatches through a
   protocol defined in ANOTHER namespace, which cljrs cannot yet do. Reader
   conditionals here must be TOTAL. See hive-addon.mount.portable-test."
  (:require [hive-addon.opaque.codec :as codec]
            [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Optional protocol methods
;; =============================================================================

(defn optional
  "Call `f` on `addon`, answering `fallback` when the addon does not implement
   that method.

   `excluded-tools` and `hooks` are optional per the protocol, and the host
   registry already defaults them for a legacy addon. The kernel must default
   them the same way, or an addon that mounts in-process would stop mounting the
   moment it is compiled opaque, which is exactly the regression a marketplace
   artifact cannot afford.

   An error that is NOT a missing method is rethrown: a broken `hooks` must not
   masquerade as an absent one."
  [addon f fallback]
  (try
    (let [v (f addon)]
      (if (nil? v) fallback v))
    ;; TOTAL reader conditional: :clj covers the JVM and cljw (both have
    ;; Throwable), :default covers cljs and cljrs. A non-total one would elide
    ;; the catch on an unlisted host and turn the graceful default into a crash.
    (catch #?(:clj Throwable :default :default) t
      (if (proto/unimplemented-method? t)
        fallback
        (throw t)))))

;; =============================================================================
;; describe: the whole pure IAddon surface, as data
;; =============================================================================

(defn describe
  "Project `addon`'s pure surface onto the wire.

   Everything here is a value the host can act on without another round trip:
   identity, type, capabilities, the tool list minus its handlers, the exclusion
   set, the schema extensions, and how each hook travels. The proxy rebuilds the
   callable half (tool handlers, fn-valued hooks) as callbacks over this wire."
  [addon]
  {:addon/id                (proto/addon-id addon)
   :addon/type              (proto/addon-type addon)
   :addon/capabilities      (set (optional addon proto/capabilities #{}))
   :addon/tools             (mapv codec/tool-summary (optional addon proto/tools []))
   :addon/excluded-tools    (set (optional addon proto/excluded-tools #{}))
   :addon/schema-extensions (codec/edn-safe (optional addon proto/schema-extensions []))
   :addon/hooks             (reduce-kv (fn [acc k v] (assoc acc k (codec/hook-summary v)))
                                       {}
                                       (optional addon proto/hooks {}))})

;; =============================================================================
;; The op handlers
;; =============================================================================

(defn- tool-handler
  "The :handler of the addon tool named `tool-name`, or nil when there is none."
  [addon tool-name]
  (->> (optional addon proto/tools [])
       (filter (fn [t] (= tool-name (:name t))))
       (map :handler)
       (filter some?)
       first))

(defn- call-tool
  [addon op args]
  (let [tool-name (:tool args)
        handler   (tool-handler addon tool-name)]
    (if (nil? handler)
      (codec/error op (str "no such tool: " (pr-str tool-name)))
      (codec/ok op (handler (codec/normalize-args (:params args)))))))

(defn- call-hook
  "Answer one :addon/hook request.

   The data-or-callable decision is `codec/safe?`, the SAME judgement
   `codec/hook-summary` used when describing the hook, and that identity is the
   whole point. `ifn?` was the first spelling and it was wrong: a map is ifn?,
   so a hook that is a lookup table was announced to the host as :data and then
   INVOKED with zero args when a host asked for it by key. The tri-host oracle
   caught it as a per-host error message, which is what a divergence in an error
   string usually is, a real defect wearing host clothing."
  [addon op args]
  (let [hook-key (:key args)
        hooks    (optional addon proto/hooks {})]
    (if-not (contains? hooks hook-key)
      (codec/error op (str "no such hook: " (pr-str hook-key)))
      (let [hook (get hooks hook-key)]
        (cond
          (codec/safe? hook) (codec/ok op hook)
          (ifn? hook)        (codec/ok op (apply hook (or (:args args) [])))
          :else              (codec/error op (str "hook is neither data nor callable: "
                                                  (pr-str hook-key))))))))

(defn handle
  "Dispatch one decoded request against `addon`, answering a response MAP.

   The op set is closed (codec/ops), so this `case` is total by construction and
   its default branch reports a vocabulary mismatch rather than a bug: it is
   what a kernel says to a host built against a newer proxy."
  [addon request]
  (let [op   (:op request)
        args (:args request)]
    (case op
      :addon/describe    (codec/ok op (describe addon))
      :addon/initialize! (codec/ok op (proto/initialize! addon (or args {})))
      :addon/shutdown!   (do (proto/shutdown! addon) (codec/ok op nil))
      :addon/health      (codec/ok op (proto/health addon))
      :addon/tool        (call-tool addon op args)
      :addon/hook        (call-hook addon op args)
      (codec/error (str "unknown op: " (pr-str op))))))

(defn respond
  "Read one request line and answer the response MAP, never throwing.

   Three failures are turned into responses rather than propagated, because a
   kernel that dies on a bad line takes the whole addon down with it: an
   unreadable line, a line that is not a request map, and a throw from inside
   the addon's own method. Only the last can echo an op."
  [addon line]
  (let [request (try (codec/decode line)
                     (catch #?(:clj Throwable :default :default) _ ::unreadable))]
    (cond
      (= ::unreadable request) (codec/error "unreadable request line")
      (not (map? request))     (codec/error "request line is not a map")
      :else
      (try
        (handle addon request)
        (catch #?(:clj Throwable :default :default) t
          (codec/error (:op request) (or (ex-message t) (str t))))))))

(defn handle-line
  "Read one request line and answer one response LINE. The whole kernel protocol
   in one function, for a host that owns its own loop."
  [addon line]
  (codec/encode (respond addon line)))

;; =============================================================================
;; The loop a compiled kernel runs
;; =============================================================================

(defn serve!
  "Serve `addon` over stdin/stdout until EOF or a shutdown request.

   Call this AT TOP LEVEL in the kernel entry namespace, not from a `-main`.
   Measured on cljw: a `build` artifact receives no *command-line-args* and its
   main is never invoked, while a top-level `read-line` loop works. An entry
   that defines an uncalled -main compiles cleanly and then does nothing, which
   is the worst possible failure for a shipped binary.

   The loop stops after answering :addon/shutdown!, so the client closing the
   pipe is not the only way for the process to end."
  [addon]
  (loop []
    (let [line (read-line)]
      (when (some? line)
        (let [response (respond addon line)]
          (println (codec/encode response))
          (flush)
          (when-not (= :addon/shutdown! (:op response))
            (recur)))))))
