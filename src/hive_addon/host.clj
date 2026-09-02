(ns hive-addon.host
  "Soft resolution of HOST services an addon consumes.

   An addon is published to maven; its host is not. A load-time
   `(:require [hive-mcp.x :as y])` therefore makes the published artifact
   unloadable wherever the host is absent. This namespace is the seam that
   replaces such a require: the host var is resolved THROUGH THE VAR at call
   time, and a missing host degrades to a Result instead of a load failure.

   Surface:
     resolve-var  symbol -> var | nil, never throws
     available?   symbol -> boolean
     soft         symbol -> fn, the per-call seam
     defsoft      def a soft fn at a call site
     api          {k symbol} -> {k soft-fn}, a var-map of them

   JVM only (.clj): `requiring-resolve` has no ClojureScript counterpart, and
   the host boundary this covers is a JVM boundary."
  (:require [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Resolution
;; =============================================================================

(defn resolve-var
  "Resolve `sym` (a fully-qualified symbol) to its var, or nil.

   Never throws: an absent namespace, a namespace that fails to load, and a
   namespace that loads without defining the name all return nil."
  [sym]
  (try (requiring-resolve sym) (catch Throwable _ nil)))

(defn available?
  "True when `sym` resolves to a var right now."
  [sym]
  (some? (resolve-var sym)))

;; =============================================================================
;; The seam
;; =============================================================================

(defn soft
  "Return a variadic fn standing in for the host fn named by `sym`.

   Each call resolves `sym` and, on success, invokes the VAR, so a host that
   loads after the addon is picked up and a host namespace reload is seen by
   the next call. A successful resolution is cached; an unsuccessful one is
   not, so absence never becomes permanent.

   When the host is absent the call returns
   `(r/err :host/absent {:host/sym <sym-string>})`, or, given `absent-fn`,
   `(apply absent-fn args)`."
  ([sym] (soft sym nil))
  ([sym absent-fn]
   (let [cache (volatile! nil)]
     (fn [& args]
       (if-let [v (or @cache (vreset! cache (resolve-var sym)))]
         (apply v args)
         (if absent-fn
           (apply absent-fn args)
           (r/err :host/absent {:host/sym (str sym)})))))))

(defmacro defsoft
  "Def `name` as the soft fn for host symbol `sym`.

   `sym` is evaluated, so quote it: (defsoft edges 'hive-mcp.kg/edges).
   Options: `:absent`, a fn called with the same args when the host is absent;
   `:doc`, a docstring."
  [name sym & {:keys [absent doc]}]
  `(def ~(with-meta name {:doc (or doc (str "Soft-resolved host fn: " sym))})
     (soft ~sym ~absent)))

(defn api
  "Build a var-map of soft fns from `{k sym}` or `{k [sym absent-fn]}`.

   The map is the whole host surface one addon namespace consumes, in one
   place; every value obeys `soft`'s contract."
  [spec]
  (reduce-kv (fn [acc k v]
               (assoc acc k (if (sequential? v)
                              (soft (first v) (second v))
                              (soft v))))
             {}
             spec))
