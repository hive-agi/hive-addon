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

;; Listeners notified after every contribute!/retract!/retract-all!.
;; Shape: {listener-id (fn [{:type :contribute|:retract :tool-name s :addon-id a
;;                          :commands [..]}])}
(defonce ^:private listeners (atom {}))

(defn add-listener!
  "Register `f`, called with a contribution event after every mutation here.

   This seam exists because the STORE alone is not the whole concept. A host
   advertises a tool surface built from these contributions, and a contribution
   that arrives AFTER that surface was built has to tell it so. While only the
   host had listeners, an addon that migrated to this registry kept working at
   boot (the surface is assembled later anyway) and silently failed for any
   post-boot contribution, which is the worst shape of failure: correct on the
   path everyone tests, wrong on the path nobody does.

   Idempotent by id: re-registering an id replaces its fn rather than stacking.

   `f` MUST NOT THROW. This namespace is in the portable three-host stratum and
   carries no reader conditionals, so it cannot catch a platform exception class
   on the caller's behalf. A listener that can fail wraps itself; a JVM host
   registers an already-guarded fn. A throwing listener will propagate out of
   the contribute!/retract! that triggered it."
  [listener-id f]
  (swap! listeners assoc listener-id f)
  listener-id)

(defn remove-listener!
  "Drop a listener. Returns the id. Safe when absent."
  [listener-id]
  (swap! listeners dissoc listener-id)
  listener-id)

(defn listener-ids
  "Every registered listener id."
  []
  (vec (keys @listeners)))

(defn- notify!
  "Call every listener with EVENT, in no guaranteed order.

   No try/catch here on purpose. This namespace is in the three-host portable
   stratum, which carries ZERO reader conditionals, and catching a platform
   exception class needs one. So isolation is the REGISTRANT's job: a listener
   that can throw must wrap itself, and a host with a JVM to catch on should
   register an already-guarded fn. Stated in add-listener!'s contract because a
   rule that lives only in a commit message is not a rule."
  [event]
  (doseq [[_ f] @listeners]
    (f event))
  nil)

(defn contribute!
  "Merge COMMANDS into TOOL-NAME's command tree on behalf of ADDON-ID.
   Notifies listeners afterwards. Returns the contributed command names."
  [tool-name addon-id commands]
  (let [tagged (tag-addon addon-id commands)
        names  (vec (keys tagged))]
    (swap! contributions update tool-name merge tagged)
    (notify! {:type :contribute :tool-name tool-name :addon-id addon-id
              :commands names})
    names))

(defn retract!
  "Remove every command ADDON-ID contributed to TOOL-NAME. Notifies listeners
   afterwards. Returns nil."
  [tool-name addon-id]
  (swap! contributions update tool-name
         (fn [m] (into {} (remove #(= addon-id (:addon (val %)))) m)))
  (notify! {:type :retract :tool-name tool-name :addon-id addon-id})
  nil)

(defn retract-all!
  "Remove every command ADDON-ID contributed to any tool. Notifies listeners
   once per tool the addon had actually touched. Returns nil.

   The touched set is read BEFORE the retraction, or there is nothing left to
   attribute the notifications to."
  [addon-id]
  (let [touched (into [] (keep (fn [[tool cmds]]
                                 (when (some #(= addon-id (:addon (val %))) cmds)
                                   tool)))
                      @contributions)]
    (swap! contributions
           (fn [m]
             (into {}
                   (map (fn [[tool cmds]]
                          [tool (into {} (remove #(= addon-id (:addon (val %)))) cmds)]))
                   m)))
    (doseq [tool touched]
      (notify! {:type :retract :tool-name tool :addon-id addon-id}))
    nil))

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
