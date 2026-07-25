(ns hive-addon.registry.extension
  "Opaque capability registry.

   An addon registers an implementation under a keyword key at startup; a
   consumer looks it up without knowing which addon provides it:

     (register! :gs/struct-cmp my-cmp-fn)

     (if-let [f (get-extension :gs/struct-cmp)]
       (f a b)
       fallback)

   Registration is atomic and idempotent — re-registering a key replaces it.")

;; SPDX-License-Identifier: MIT

(defonce ^:private extensions (atom {}))

(defn register!
  "Register F under keyword K. Returns K."
  [k f]
  {:pre [(keyword? k) (ifn? f)]}
  (swap! extensions assoc k f)
  k)

(defn register-many!
  "Register every entry of M, a {keyword fn} map, in one swap. Returns the
   registered keys."
  [m]
  {:pre [(map? m)]}
  (swap! extensions merge m)
  (keys m))

(defn get-extension
  "The function registered under K, or DEFAULT (nil when not given)."
  ([k] (get @extensions k))
  ([k default] (get @extensions k default)))

(defn extension-available?
  "True iff something is registered under K."
  [k]
  (contains? @extensions k))

(defn registered-keys
  "The set of registered extension keys."
  []
  (set (keys @extensions)))

(defn deregister!
  "Remove the registration under K. Returns K."
  [k]
  (swap! extensions dissoc k)
  k)

(defn clear!
  "Remove every extension registration. Returns nil."
  []
  (reset! extensions {})
  nil)
