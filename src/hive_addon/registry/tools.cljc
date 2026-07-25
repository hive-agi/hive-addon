(ns hive-addon.registry.tools
  "Dynamically registered tool definitions.

   A tool definition is a map carrying at least a :name string and a
   :handler fn. Registration is keyed by :name, last write wins.")

;; SPDX-License-Identifier: MIT

(defonce ^:private tools (atom {}))

(defn register!
  "Register TOOL-DEF under its :name. Returns that name."
  [tool-def]
  {:pre [(string? (:name tool-def)) (ifn? (:handler tool-def))]}
  (swap! tools assoc (:name tool-def) tool-def)
  (:name tool-def))

(defn registered
  "Every registered tool definition."
  []
  (vec (vals @tools)))

(defn registered-names
  "The name of every registered tool."
  []
  (vec (keys @tools)))

(defn get-tool
  "The tool definition registered under TOOL-NAME, or nil."
  [tool-name]
  (get @tools tool-name))

(defn deregister!
  "Remove the tool registered under TOOL-NAME. Returns that name."
  [tool-name]
  (swap! tools dissoc tool-name)
  tool-name)

(defn clear!
  "Remove every tool registration. Returns nil."
  []
  (reset! tools {})
  nil)
