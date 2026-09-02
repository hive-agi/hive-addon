(ns hive-addon.opaque.codec
  "The OPAQUE wire codec: the one place the request/response bytes are decided,
   and the only opaque namespace BOTH sides load.

   A proprietary addon ships as a compiled kernel in its own process; the host
   mounts a generic proxy that speaks to it over one EDN line per request and
   one per response. The proxy frames with `request`/`encode` and reads with
   `decode`; the kernel reads with `decode` and frames with `ok`/`error`. One
   namespace writes both sides, so proxy and kernel cannot drift.

   MALLI-FREE ON PURPOSE. The kernel is compiled by `cljw build` into a native
   binary, and malli does not load there; the schemas that describe these shapes
   (hive-addon.opaque.schema) and the contracts over them
   (hive-addon.opaque.contracts) are a HOST-side boundary layer. Nothing here
   validates: validation is the host's job, on values this namespace produced.

   Three-host portable stratum, so no reader conditionals, no `for`, no bare
   record fields. See hive-addon.mount.portable-test."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; The op vocabulary, closed because IAddon's method set is closed
;; =============================================================================

(def ops
  "Every verb the proxy may send. CLOSED: these are the IAddon methods that
   survive a process boundary, and IAddon's method set is closed, so this set is
   closed with it. A new op means a new protocol method, not a config entry."
  #{:addon/describe
    :addon/initialize!
    :addon/shutdown!
    :addon/health
    :addon/tool
    :addon/hook})

(defn op?
  "True when x is one of the closed wire ops."
  [x]
  (contains? ops x))

;; =============================================================================
;; The wire vocabulary: what a value may be by the time it is framed
;; =============================================================================

(defn- scalar-safe?
  "True for a scalar the wire carries. Doubles exclude NaN and the infinities:
   they print, but they do not round-trip through equality, so an oracle that
   compares a decoded value to the encoded one would report a false divergence.

   The exclusion is stated in ARITHMETIC, not host interop. NaN is the only
   value not equal to itself, and an infinity is the only non-zero value equal
   to half of itself. `Double/isNaN` would be a JVM class reference in a
   namespace that must also load on cljw and cljrs."
  [x]
  (or (nil? x)
      (string? x)
      (boolean? x)
      (keyword? x)
      (symbol? x)
      (integer? x)
      (and (number? x)
           (== x x)
           (or (zero? x) (not (== x (/ x 2)))))))

(defn safe?
  "True when x crosses the wire UNCHANGED: a scalar in the vocabulary, or a
   plain collection every part of which is itself safe. A function, an atom, a
   live addon instance or a host object is not safe.

   `safe?` and `edn-safe` are two halves of one statement, and the law that ties
   them is `(safe? x)` implies `(= x (edn-safe x))`. A RECORD is therefore not
   safe even when every field is: EDN has no reader for it, so it crosses as a
   plain map and comes back as one, which is a change."
  [x]
  (cond
    (record? x) false

    (map? x)
    (reduce-kv (fn [acc k v] (and acc (safe? k) (safe? v))) true x)

    (or (set? x) (vector? x) (sequential? x))
    (reduce (fn [acc v] (and acc (safe? v))) true x)

    :else (scalar-safe? x)))

(defn edn-safe
  "Project x onto the wire vocabulary by DROPPING what cannot cross it.

   Dropping happens at the LEAF. A collection is always recursed into, so one
   unusable value costs that value and nothing around it: a config
   {:opts {:tuning 0.5 :cb <fn>}} crosses as {:opts {:tuning 0.5}} rather than
   losing :opts entirely. Gating the whole subtree on its worst leaf was the
   first shape of this function and it was wrong in a way that only shows up on
   a real mount config, where one live dependency sits beside data the kernel
   needs. A map key is the exception: it is dropped whole, because a key cannot
   be partially rewritten without becoming a different key.

   This is how a mount config crosses at all. :mount/dependencies holds live
   addon instances, and the kernel gets an empty map there and the rest intact,
   rather than a serialization failure.

   Dropping rather than nilling is deliberate. A vector of live objects is
   better as an empty vector than as a vector of nils, which the kernel cannot
   tell apart from a genuine nil the caller sent. The cost is that positions
   shift, which is why anything positional belongs in a map.

   Total: EVERY value has a projection, because an unusable scalar projects to
   nil rather than throwing. That is what makes it safe to run over a config
   nobody has inspected."
  [x]
  (letfn [(keep? [v] (or (coll? v) (safe? v)))]
    (cond
      (map? x)
      (reduce-kv (fn [acc k v]
                   (if (and (safe? k) (keep? v))
                     (assoc acc k (edn-safe v))
                     acc))
                 {}
                 x)

      (set? x)
      (into #{} (comp (filter keep?) (map edn-safe)) x)

      (or (vector? x) (sequential? x))
      (into [] (comp (filter keep?) (map edn-safe)) x)

      (scalar-safe? x) x

      :else nil)))

;; =============================================================================
;; Framing: one line in, one line out
;; =============================================================================

(defn encode
  "Frame a request or response map as ONE wire line (no trailing newline).
   Printer vars are pinned, because the bytes must be a function of the value
   alone: a *print-length* bound elsewhere in a host would silently truncate a
   tool result into a syntactically valid but WRONG line."
  [v]
  (binding [*print-length*         nil
            *print-level*          nil
            *print-namespace-maps* false
            *print-readably*       true]
    (pr-str v)))

(defn decode
  "Read one wire line back into a request or response map. nil and a blank line
   decode to nil, which is how EOF and a keepalive newline are told apart from
   content.

   A MALFORMED line THROWS, and that is deliberate. Catching it here would need
   a catch class, which needs a reader conditional, which is exactly what keeps
   a namespace out of the three-host stratum. The two places that read a line
   are boundaries that already own a rescue: hive-addon.opaque.serve turns the
   throw into an :error response line, and hive-addon.opaque.addon turns it into
   a failed call. Neither can delegate that duty here without dragging host
   interop into the one namespace the kernel and the host share.

   The :default reader keeps an unknown tagged literal's VALUE instead of
   throwing, so a kernel built against a newer vocabulary degrades to data."
  [line]
  (when (and (string? line) (not (str/blank? line)))
    (edn/read-string {:default (fn [_tag v] v)} line)))

;; =============================================================================
;; The message constructors: the only shapes that go on the wire
;; =============================================================================

(defn request
  "Build one request. `args` is projected onto the wire vocabulary, so a caller
   may hand it a live config map; it is omitted entirely when nil."
  ([op] {:op op})
  ([op args] (if (nil? args) {:op op} {:op op :args (edn-safe args)})))

(defn ok
  "Build a success response echoing `op`. The result is projected onto the wire
   vocabulary, so a kernel that returns a function returns nothing instead."
  [op result]
  {:op op :result (edn-safe result)})

(defn error
  "Build an error response. The single-arity form is for a line that failed to
   parse: there is no op to echo, and inventing one would misattribute it."
  ([message] {:error (str message)})
  ([op message] {:op op :error (str message)}))

(defn error?
  "True when a decoded response is an error rather than a result."
  [response]
  (and (map? response) (contains? response :error)))

(defn result
  "The :result of a decoded response, or nil for an error or garbage response.
   A caller that must tell a nil result apart from a failure checks `error?`."
  [response]
  (when (and (map? response) (not (error? response)))
    (:result response)))

;; =============================================================================
;; Projections both sides agree on
;; =============================================================================

(defn normalize-args
  "Keywordize an argument map's keys. MCP hands a tool string keys and an
   in-process caller hands it keywords; the kernel must see one shape."
  [params]
  (reduce-kv (fn [acc k v] (assoc acc (if (string? k) (keyword k) k) v))
             {}
             (if (map? params) params {})))

(defn tool-summary
  "Project an IAddon tool-def onto the wire by removing its :handler. The fn is
   exactly what cannot cross; the proxy supplies one that calls back."
  [tool-def]
  (edn-safe (dissoc tool-def :handler)))

(defn hook-summary
  "Project one hook value onto the wire. A fn-valued hook is announced as :fn,
   and the proxy installs a fn that calls back; a data-valued hook travels as
   its value, so a hook that is a lookup table stays a lookup table."
  [hook-value]
  (if (safe? hook-value)
    {:kind :data :value (edn-safe hook-value)}
    {:kind :fn}))

(defn health-report
  "Build an IAddon health map from a liveness flag and a details map."
  [alive? details]
  {:status (if alive? :ok :down) :details (edn-safe (or details {}))})

(defn init-result
  "Build an IAddon initialize! result map."
  ([ok?] (init-result ok? []))
  ([ok? errors] {:success? (boolean ok?) :errors (vec errors)}))
