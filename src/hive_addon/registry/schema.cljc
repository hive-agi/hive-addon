(ns hive-addon.registry.schema
  "Schema-property extensions contributed to a host tool.

   An addon widens an existing tool's input schema by registering extra
   properties under that tool's name. Contributions merge, so several addons
   may extend the same tool.")

;; SPDX-License-Identifier: MIT

(defonce ^:private schemas (atom {}))

(defn register!
  "Merge PROPERTIES, a {\"param\" {:type ... :description ...}} map, into the
   schema extensions for TOOL-NAME. Returns TOOL-NAME."
  [tool-name properties]
  {:pre [(string? tool-name) (map? properties)]}
  (swap! schemas update tool-name merge properties)
  tool-name)

(defn get-extensions
  "The merged schema properties for TOOL-NAME, or nil."
  [tool-name]
  (get @schemas tool-name))

(defn extended-tool-names
  "The names of every tool carrying schema extensions."
  []
  (vec (keys @schemas)))

(defn clear!
  "Remove every schema extension. Returns nil."
  []
  (reset! schemas {})
  nil)
