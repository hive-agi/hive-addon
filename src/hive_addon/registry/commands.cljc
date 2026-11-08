(ns hive-addon.registry.commands
  "Command contributions to a composite tool.

   An addon contributes named subcommands to a host tool, so
   `analysis command=\"lint ...\"` reaches the contributing addon's handler.
   Each contribution records the contributing addon so shutdown can retract
   everything that addon added.

   Shape: {tool-name {command-name {:handler fn :params {} :description str
                                    :addon addon-id}}}"
  (:require [hive-addon.registry.branches :as branches]))

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

;; Contributions made by NAME: {[tool-name addon-id] contribution-sym}.
;; What refresh-namespaces! re-resolves after the symbol's namespace reloads.
(defonce ^:private sources (atom {}))

(defn contribute-from!
  "Like contribute!, but the command map is the value of the var named by
   CONTRIBUTION-SYM, read through RESOLVE-FN (sym -> value, or nil). The name
   is remembered, so refresh-namespaces! can re-read it after that namespace
   is reloaded in place: handlers in the map then close over the NEW vars.
   Returns the contributed command names, or nil when the symbol does not
   resolve (nothing is contributed)."
  [tool-name addon-id contribution-sym resolve-fn]
  (when-let [commands (resolve-fn contribution-sym)]
    (swap! sources assoc [tool-name addon-id] contribution-sym)
    (contribute! tool-name addon-id commands)))

(defn retract!
  "Remove every command ADDON-ID contributed to TOOL-NAME. Notifies listeners
   afterwards. Returns nil."
  [tool-name addon-id]
  (swap! contributions update tool-name
         (fn [m] (into {} (remove #(= addon-id (:addon (val %)))) m)))
  (notify! {:type :retract :tool-name tool-name :addon-id addon-id})
  nil)

(declare drop-owner-branches!)

(defn retract-all!
  "Remove every command ADDON-ID contributed to any tool, and every runtime branch
   it registered. Notifies listeners once per tool the addon had actually touched.
   Returns nil.

   The touched set is read BEFORE the retraction, or there is nothing left to
   attribute the notifications to."
  [addon-id]
  (let [touched (into [] (keep (fn [[tool cmds]]
                                 (when (some #(= addon-id (:addon (val %))) cmds)
                                   tool)))
                      @contributions)
        branched (drop-owner-branches! addon-id)]
    (swap! contributions
           (fn [m]
             (into {}
                   (map (fn [[tool cmds]]
                          [tool (into {} (remove #(= addon-id (:addon (val %)))) cmds)]))
                   m)))
    (doseq [tool (distinct (concat touched branched))]
      (notify! {:type :retract :tool-name tool :addon-id addon-id}))
    nil))

;; =============================================================================
;; Runtime branches: see hive-addon.registry.branches
;; =============================================================================

;; {tool {[kind path owner] Branch}}
(defonce ^:private branch-store (atom {}))

;; {tool [Refusal]} as of the tool's latest composition
(defonce ^:private refused-now (atom {}))

;; {:tool-handler (fn [tool]) :routes (fn [tool]) -> #{command} :guard (fn [b t f])}
(defonce ^:private branch-host (atom nil))

;; {tool #{command}}: the host's own routes, asked once per branch change
(defonce ^:private route-cache (atom {}))

;; {tool #{path}}: paths no around may cover
(defonce ^:private protected-paths (atom {}))

(def ^:dynamic *bypass*
  "Tools whose branches are off on this thread: a re-entry into the host's own verb."
  #{})

(defn- owner-id [owner]
  (cond (keyword? owner) (subs (str owner) 1)
        (nil? owner)     nil
        :else            (str owner)))

(defn- changed! [tool type owner]
  (swap! route-cache dissoc tool)
  (notify! {:type type :tool-name tool :addon-id owner}))

(defn drop-owner-branches!
  "Drop every branch OWNER registered. Returns the tools it touched."
  [owner]
  (let [o     (owner-id owner)
        found (vec (mapcat (fn [[tool bs]]
                             (keep (fn [[k b]] (when (= o (:owner b)) [tool k])) bs))
                           @branch-store))]
    (doseq [[tool k] found] (swap! branch-store update tool dissoc k))
    (doseq [tool (distinct (map first found))] (swap! route-cache dissoc tool))
    (vec (distinct (map first found)))))

(defn set-branch-host!
  "Tell the store how to reach a tool from outside: a map of
   :tool-handler (fn [tool]) -> its handler,
   :routes (fn [tool]) -> the command strings it routes, called inside `*bypass*`,
   :guard (fn [branch tool f]) -> f as dispatched.
   Without one, a branch can only touch a root an addon contributed."
  [host]
  (reset! branch-host host)
  (reset! route-cache {})
  nil)

(defn branches-of
  "Every branch registered on TOOL."
  [tool]
  (vec (vals (get @branch-store tool))))

(defn branch-refusals
  "The branches of TOOL that did not route at its latest composition."
  [tool]
  (get @refused-now tool []))

(defn protect!
  "Refuse any around over PATH of TOOL, or over a path containing it, so the verb
   that removes a broken around stays reachable."
  [tool path]
  (swap! protected-paths update tool (fnil conj #{}) (branches/parse-path path))
  nil)

(defn- covers-protected?
  "Does an around over P touch a protected path: above it, at it, or below it?"
  [tool p]
  (some (fn [q]
          (let [n (min (count p) (count q))]
            (= (subvec p 0 n) (subvec q 0 n))))
        (get @protected-paths tool)))

(defn graft!
  "Route PATH of TOOL to SPEC's :handler, a handler or verb map, for its :owner.
   Refused, with nothing written, for an empty path, an uninvocable handler, a blank
   owner, or a leaf another owner grafted. The same owner replaces. A route that
   exists is judged per composition: `branch-refusals` names it.
   Returns {:ok? true ...} or {:ok? false :reason kw :message s}."
  [tool path {:keys [handler owner] :as spec}]
  (let [p      (branches/parse-path path)
        o      (owner-id owner)
        label  (str tool " " (branches/path-str p))
        node   (branches/current handler)
        leaves (when (or (branches/handler? handler) (and (map? node) (seq node)))
                 (map first (branches/flatten-graft p handler)))
        other  (some (fn [{other :owner :as b}]
                       (when (and (= :graft (:kind b)) (not= o other)
                                  (some (set leaves) (map first (branches/flatten-graft (:path b) (:handler b)))))
                         other))
                     (branches-of tool))]
    (cond
      (empty? p)    {:ok? false :reason :branch/empty-path :message (str tool ": a graft needs a path")}
      (nil? leaves) {:ok? false :reason :branch/not-invocable :message (str label ": handler is not invocable")}
      (nil? o)      {:ok? false :reason :branch/no-owner :message (str label ": owner is required")}
      other         {:ok? false :reason :branch/owned-by :owner other :message (str label " is already grafted by " other)}
      :else
      (do (swap! branch-store assoc-in [tool [:graft p o]]
                 (assoc (select-keys spec [:handler :params :summary :description])
                        :kind :graft :path p :owner o))
          (changed! tool :branch o)
          {:ok? true :tool tool :path (branches/path-str p) :owner o}))))

(defn around!
  "Wrap PATH of TOOL, and everything below it, in SPEC's :wrap, (fn [next] handler);
   an empty path wraps the whole tool. Several owners may wrap one path; lowest :order
   runs outermost; the same owner replaces. Refused over a `protect!`ed path.
   Returns {:ok? true ...} or {:ok? false :reason kw :message s}."
  [tool path {:keys [wrap owner] :as spec}]
  (let [p     (branches/parse-path path)
        o     (owner-id owner)
        label (str tool " " (branches/path-str p))]
    (cond
      (not (branches/handler? wrap)) {:ok? false :reason :branch/not-invocable :message (str label ": wrap must be (fn [next] handler)")}
      (nil? o)                       {:ok? false :reason :branch/no-owner :message (str label ": owner is required")}
      (covers-protected? tool p)     {:ok? false :reason :branch/protected :message (str label " covers a protected path")}
      :else
      (do (swap! branch-store assoc-in [tool [:around p o]]
                 (assoc (select-keys spec [:wrap :order :params :summary :description])
                        :kind :around :path p :owner o))
          (changed! tool :branch o)
          {:ok? true :tool tool :path (branches/path-str p) :owner o}))))

(defn prune!
  "Withdraw OWNER's branches at exactly PATH of TOOL. Returns how many went."
  [tool path owner]
  (let [p  (branches/parse-path path)
        o  (owner-id owner)
        ks (filter (partial contains? (get @branch-store tool {})) [[:graft p o] [:around p o]])]
    (when (seq ks)
      (swap! branch-store update tool #(apply dissoc % ks))
      (changed! tool :prune o))
    (count ks)))

(defn clear-branches!
  "Drop every branch, refusal, protection and the host. For tests."
  []
  (reset! branch-store {})
  (reset! refused-now {})
  (reset! protected-paths {})
  (reset! route-cache {})
  (reset! branch-host nil)
  nil)

(defn- routes-of
  "The host's own routes for TOOL, nil when no host can say. Asked with this tool's
   branches off, once per branch change."
  [tool]
  (when-let [routes (:routes @branch-host)]
    (or (get @route-cache tool)
        (let [found (binding [*bypass* (conj *bypass* tool)] (routes tool))]
          (when found (swap! route-cache assoc tool (set found)))
          (when found (set found))))))

(defn- next-handler
  "The re-entry that reaches TOOL's own verb: the tool itself, branches off for it on
   this thread. The marker stops a host that loses the binding across threads from
   recursing: it answers an error instead."
  [tool]
  (fn [params]
    (if (:__branch_next params)
      {:type "text" :isError true
       :text (str tool ": a branch re-entered itself; this host dispatches off the calling thread")}
      (if-let [h (some-> (:tool-handler @branch-host) (#(% tool)))]
        (binding [*bypass* (conj *bypass* tool)]
          (h (assoc params :__branch_next true)))
        {:type "text" :isError true :text (str "no handler reachable for tool " tool)}))))

(defn refresh-namespaces!
  "Re-run every contribute-from! whose contribution symbol lives in one of
   NS-STRS (reloaded namespace names) and that is still contributed. Each is
   retracted and contributed again from (RESOLVE-FN sym), keeping its addon
   attribution, so listeners see the change. Returns {tool-name [command ...]}
   of what was refreshed; {} when nothing applies."
  [ns-strs resolve-fn]
  (let [reloaded (set (map str ns-strs))
        live?    (fn [[tool addon-id]]
                   (some #(= addon-id (:addon (val %))) (get @contributions tool)))]
    (reduce (fn [acc [[tool addon-id :as k] sym]]
              (if (and (contains? reloaded (namespace sym)) (live? k))
                (if-let [commands (resolve-fn sym)]
                  (do (retract! tool addon-id)
                      (update acc tool (fnil into [])
                              (contribute! tool addon-id commands)))
                  acc)
                acc))
            {}
            @sources)))

(defn get-commands
  "The contributed command tree for TOOL-NAME with its runtime branches composed in,
   or nil. Raw inside `*bypass*`. Composed per read, so a branch survives a remount of
   the addon it extends and a reload of either side is seen by the next read."
  [tool-name]
  (let [raw (get @contributions tool-name)
        bs  (when-not (contains? *bypass* tool-name) (seq (branches-of tool-name)))]
    (if-not bs
      raw
      (let [{:keys [specs refusals]}
            (branches/compose raw bs tool-name
                              {:routes       (routes-of tool-name)
                               :next-handler (next-handler tool-name)
                               :guard        (:guard @branch-host)})]
        (swap! refused-now assoc tool-name refusals)
        specs))))

(defn forget-composition!
  "Forget that TOOL composed its branches, so `composed?` answers for the next read."
  [tool]
  (swap! refused-now dissoc tool)
  nil)

(defn composed?
  "Has anything read TOOL's composed commands since `forget-composition!`? A tool that
   took a call and did not never reads this store, and no branch can reach it."
  [tool]
  (contains? @refused-now tool))

(defn contributed-tool-names
  "Every tool name carrying contributions or branches."
  []
  (vec (distinct (concat (keys @contributions)
                         (keep (fn [[tool bs]] (when (seq bs) tool)) @branch-store)))))

(defn clear!
  "Remove every contribution. Returns nil."
  []
  (reset! contributions {})
  nil)
