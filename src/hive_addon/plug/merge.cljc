(ns hive-addon.plug.merge
  "Fold iaddon config layers into one, with the trust security inversion applied to
   :iaddon/trust so higher-precedence layers can only tighten, never loosen."
  (:require [clojure.set :as set]))

;; SPDX-License-Identifier: MIT

(defn deep-merge
  "Recursively merge maps: later wins per key, nested maps recurse, others replace."
  [& maps]
  (letfn [(m2 [a b] (if (and (map? a) (map? b)) (merge-with m2 a b) b))]
    (reduce m2 nil (remove nil? maps))))

(defn- union-required
  "Signature requirements grow monotonically across layers."
  [trusts]
  (reduce into #{} (keep #(some-> (:require-signature %) set) trusts)))

(defn- seeded-allow
  "Unsigned grants are seeded by the lowest layer and only intersected (tightened) by
   higher layers; required classes are always removed."
  [trusts required]
  (let [seed  (or (some-> (:allow-unsigned (first trusts)) set) #{})
        grant (reduce (fn [acc t] (if-let [au (some-> (:allow-unsigned t) set)]
                                    (set/intersection acc au)
                                    acc))
                      seed (rest trusts))]
    (set/difference grant required)))

(defn- lowest-keyring
  "The trust root is pinned by the lowest declaring layer."
  [trusts]
  (some :keyring trusts))

(defn- merged-pins
  "Pins are lower-wins on conflict: a higher layer may add, never repin or drop."
  [trusts]
  (when (some :pinned trusts)
    (apply merge-with (fn [lo _hi] lo) (keep :pinned trusts))))

(defn- merge-trust [trust-layers]
  (let [ts (remove nil? trust-layers)]
    (when (seq ts)
      (let [base    (apply merge (map #(dissoc % :require-signature :allow-unsigned :keyring :pinned) ts))
            req     (union-required ts)
            allow   (seeded-allow ts req)
            keyring (lowest-keyring ts)
            pinned  (merged-pins ts)]
        (cond-> base
          keyring      (assoc :keyring keyring)
          (seq req)    (assoc :require-signature req)
          (seq allow)  (assoc :allow-unsigned allow)
          (seq pinned) (assoc :pinned pinned))))))

(defn merge-configs
  "Fold config layers (lowest precedence first) into one config; sections deep-merge,
   :iaddon/trust merges under the security inversion. Empty input yields {}."
  [configs]
  (let [cfgs  (remove nil? configs)
        base  (or (apply deep-merge cfgs) {})
        trust (merge-trust (map :iaddon/trust cfgs))]
    (cond-> base
      (some? trust) (assoc :iaddon/trust trust)
      (nil? trust)  (dissoc :iaddon/trust))))
