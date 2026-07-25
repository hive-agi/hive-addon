(ns hive-addon.registry.commands
  "Command contributions to a composite tool.

   An addon contributes named subcommands to a host tool, so
   `analysis command=\"lint ...\"` reaches the contributing addon's handler.
   Each contribution records the contributing addon so shutdown can retract
   everything that addon added.

   Shape: {tool-name {command-name {:handler fn :params {} :description str
                                    :addon addon-id}}}")

;; SPDX-License-Identifier: MIT

(defonce ^:private contributions (atom {}))

(defn- tag-addon
  "COMMANDS with :addon stamped on every spec, keys coerced to strings."
  [addon-id commands]
  (into {} (map (fn [[cmd spec]] [(name cmd) (assoc spec :addon addon-id)]))
        commands))

(defn contribute!
  "Merge COMMANDS into TOOL-NAME's command tree on behalf of ADDON-ID.
   Returns the contributed command names."
  [tool-name addon-id commands]
  (swap! contributions update tool-name merge (tag-addon addon-id commands))
  (vec (keys (tag-addon addon-id commands))))

(defn retract!
  "Remove every command ADDON-ID contributed to TOOL-NAME. Returns nil."
  [tool-name addon-id]
  (swap! contributions update tool-name
         (fn [m] (into {} (remove #(= addon-id (:addon (val %)))) m)))
  nil)

(defn retract-all!
  "Remove every command ADDON-ID contributed to any tool. Returns nil."
  [addon-id]
  (swap! contributions
         (fn [m]
           (into {}
                 (map (fn [[tool cmds]]
                        [tool (into {} (remove #(= addon-id (:addon (val %)))) cmds)]))
                 m)))
  nil)

(defn get-commands
  "The contributed command tree for TOOL-NAME, or nil."
  [tool-name]
  (get @contributions tool-name))

(defn contributed-tool-names
  "Every tool name carrying contributions."
  []
  (vec (keys @contributions)))

(defn clear!
  "Remove every contribution. Returns nil."
  []
  (reset! contributions {})
  nil)
