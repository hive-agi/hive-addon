(ns hive-addon.wire
  "Folding host values into something a serializer can encode.

   A pure calculation with no dependencies, living in the lowest lib both the
   host and its addons already depend on. It is here rather than in hive-mcp
   because an addon depends on hive-di / hive-contracts / hive-addon and NEVER
   on hive-mcp core, and hive-addon.hot needs this fold for its own reports.

   Every report that crosses a serializer needs the same fold, so there is one
   of it. Two copies of this concept is how a report starts throwing on one
   path and not the other.")

;; SPDX-License-Identifier: MIT

(defn json-safe
  "Fold arbitrary evidence into values a JSON writer can encode.

   Functions become the STABLE marker \"#fn\" rather than their printed identity:
   a report is read by diffing it against the last one, and an object whose
   rendering carries an identity hash changes on every call while meaning the
   same thing. Symbols and unknown host objects become strings, sets become
   vectors (JSON has no set) ordered by their printed form so the fold is
   deterministic, and map keys that are neither string nor keyword are printed.

   Live objects never reach the wire: that is the point of the fold, not a
   side effect of it."
  [x]
  (cond
    (or (nil? x) (string? x) (boolean? x) (number? x) (keyword? x)) x
    (symbol? x)     (str x)
    (fn? x)         "#fn"
    (map? x)        (into {}
                          (map (fn [[k v]]
                                 [(if (or (string? k) (keyword? k)) k (str k))
                                  (json-safe v)]))
                          x)
    (set? x)        (mapv json-safe (sort-by str x))
    (sequential? x) (mapv json-safe x)
    :else           (str x)))
