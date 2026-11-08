(ns hive-addon.registry.branches
  "Pure composition of runtime branches onto a tool's contributed commands.

   A BRANCH extends or wraps one path of a tool's command tree, at any depth:

     graft    `git belt hello` beside what `git belt` already routes
     around   middleware, (fn [next] handler), over a verb, a subtree or the tool

   The host re-reads contributions on every call and lets a contribution win over
   its own verb of the same name, so composing HERE, on the way out of the store,
   needs nothing from the host. Touching a root the host itself owns contributes
   that root, with a re-entry into the tool as its default route: the host's own
   verb is still what answers everything no branch took.

   A leaf that routes its own remainder is SPLIT, never replaced: it becomes
   {:_handler leaf, seg branch} and still receives the whole :command.

   A graft never shadows a route; changing one is what `around` is for.

   No reader conditionals, as for the registry above it. Catching a platform
   exception is therefore the :guard the host injects."
  (:require [clojure.string :as str]))

;; SPDX-License-Identifier: MIT

(def ^:private max-var-hops 16)

(defn current
  "The value X denotes right now, following vars to a fixed point (bounded)."
  [x]
  (loop [v x hops 0]
    (if (and (var? v) (< hops max-var-hops))
      (recur (deref v) (inc hops))
      v)))

(defn handler?
  "Is X invocable code, or a var holding it? Data is `ifn?` too and is not a handler."
  [x]
  (let [c (current x)]
    (and (ifn? c)
         (not (or (map? c) (set? c) (vector? c) (keyword? c) (symbol? c))))))

;; =============================================================================
;; Paths
;; =============================================================================

(def ^:private whitespace #{\space \tab \newline \return})

(defn tokens
  "S split on runs of whitespace, blanks dropped."
  [s]
  (into []
        (comp (remove (comp whitespace first))
              (map #(apply str %)))
        (partition-by whitespace (str s))))

(defn parse-path
  "PATH as a vector of keywords: \"belt hello\", [\"belt\" :hello] or :belt. Blank
   or nil is [], which names the whole tool."
  [path]
  (cond
    (nil? path)        []
    (keyword? path)    [path]
    (string? path)     (mapv keyword (tokens path))
    (sequential? path) (mapv #(if (keyword? %) % (keyword (str %))) path)
    :else              [(keyword (str path))]))

(defn path-str
  "PATH as the command string a caller sends."
  [path]
  (str/join " " (map name path)))

(defn flatten-graft
  "Every [path leaf] a graft of NODE at PATH makes routable. A verb map becomes one
   entry per leaf, so grafts merge key by key. A leaf stays as registered, so a var
   stays a var."
  [path node]
  (let [cur (current node)]
    (if (map? cur)
      (vec (mapcat (fn [[k v]] (flatten-graft (conj path (keyword k)) v)) cur))
      [[path node]])))

(defn- key-in
  [m seg]
  (cond (contains? m seg)        seg
        (contains? m (name seg)) (name seg)
        :else                    seg))

;; =============================================================================
;; Graft
;; =============================================================================

(defn- occupied?
  "Does TREE already route exactly PATH?"
  [tree path]
  (loop [node tree p (seq path)]
    (let [cur (current node)]
      (cond
        (empty? p) (some? cur)
        (map? cur) (let [k (key-in cur (first p))]
                     (and (contains? cur k) (recur (get cur k) (next p))))
        :else      (and (handler? cur) (= [:_handler] (vec p)))))))

(defn- declared-verb?
  "Does the opaque root of a two-segment PATH say it routes that verb? (`declared`)"
  [tree path declared]
  (when (and declared (= 2 (count path)))
    (let [[root verb] path]
      (when-not (map? (current (get tree (key-in tree root))))
        (contains? (into #{} (map name) (declared root)) (name verb))))))

(defn- assoc-path
  "NODE with LEAF at PATH. A leaf on the way down is split into the :_handler of a
   new subtree. A level created below a default inherits it, or grafting `sub a b`
   would take `sub a c` away from the handler that routes it."
  [node [seg & more :as path] leaf default]
  (if (empty? path)
    leaf
    (let [cur (current node)
          m   (cond (map? cur)      cur
                    (handler? cur)  {:_handler node}
                    (some? default) {:_handler default}
                    :else           {})
          k   (key-in m seg)]
      (assoc m k (assoc-path (get m k) more leaf (or (:_handler m) default))))))

;; =============================================================================
;; Around
;; =============================================================================

(defn- wrap-leaves
  [node wrap-leaf]
  (let [cur (current node)]
    (cond
      (map? cur)     (reduce-kv (fn [m k v] (assoc m k (wrap-leaves v wrap-leaf))) cur cur)
      (handler? cur) (wrap-leaf node)
      :else          node)))

(defn- advise-path
  "NODE with WRAP-LEAF over everything PATH names, or ::no-target. A segment no map
   lists falls to its :_handler and a leaf is split, so a verb only an opaque handler
   knows can be wrapped."
  [node [seg & more :as path] wrap-leaf]
  (let [cur (current node)]
    (cond
      (empty? path) (wrap-leaves node wrap-leaf)

      (map? cur)
      (let [k       (key-in cur seg)
            target  (if (contains? cur k) (get cur k) (:_handler cur))
            advised (if (some? target) (advise-path target more wrap-leaf) ::no-target)]
        (if (= ::no-target advised) ::no-target (assoc cur k advised)))

      (handler? cur)
      (let [advised (advise-path node more wrap-leaf)]
        (if (= ::no-target advised) ::no-target {:_handler node seg advised}))

      :else ::no-target)))

(defn failure
  "The answer of a branch that threw, for a host's :guard: who and where."
  [{:keys [kind path owner]} tool message]
  {:type    "text"
   :isError true
   :text    (str (name kind) " branch '" tool " " (path-str path) "' of " owner
                 " threw: " message)})

;; =============================================================================
;; Fold over a tree
;; =============================================================================

(defn- refuse [{:keys [kind path owner]} reason]
  {:kind kind :path path :owner owner :reason reason})

(defn- fold-graft
  [acc {:keys [path handler] :as branch} tool {:keys [declared routes guard]}]
  (reduce
   (fn [{:keys [tree] :as acc} [leaf-path leaf]]
     (let [b      (assoc branch :path leaf-path)
           reason (cond
                    (occupied? tree leaf-path)               :branch/occupied
                    (contains? routes (path-str leaf-path))  :branch/occupied
                    (declared-verb? tree leaf-path declared) :branch/shadows-verb)]
       (if reason
         (update acc :refusals conj (refuse b reason))
         (assoc acc :tree (assoc-path tree leaf-path (guard b tool leaf) nil)))))
   acc
   (flatten-graft path handler)))

(defn- fold-around
  [{:keys [tree] :as acc} {:keys [path wrap] :as branch} tool {:keys [guard]}]
  (let [wrapping (fn [next-handler]
                   (guard branch tool (fn [params] ((wrap next-handler) params))))
        advised  (advise-path tree path wrapping)]
    (if (= ::no-target advised)
      (update acc :refusals conj (refuse branch :branch/no-target))
      (assoc acc :tree advised))))

(defn fold
  "TREE with BRANCHES applied. Returns {:tree :refusals}.

   Grafts first, shallowest first, so a graft at [:a] and at [:a :b] compose in
   either arrival order. Arounds after, so one can wrap a graft. Lowest :order runs
   outermost; ties break by owner, so arrival order never decides.

   OPTS: :declared (fn [root]) -> verbs an opaque root routes; :routes #{command}
   the host routes; :guard (fn [branch tool f]) -> f as dispatched."
  [tree branches tool opts]
  (let [opts   (-> opts
                   (update :guard #(or % (fn [_ _ f] f)))
                   (update :routes #(or % #{})))
        {:keys [graft around]} (group-by :kind branches)
        grafts (sort-by (juxt (comp count :path) (comp path-str :path) :owner) graft)
        wraps  (sort-by (juxt #(- (or (:order %) 0)) :owner) around)]
    (as-> {:tree tree :refusals []} acc
      (reduce #(fold-graft %1 %2 tool opts) acc grafts)
      (reduce #(fold-around %1 %2 tool opts) acc wraps))))

;; =============================================================================
;; Compose over contributed specs: what the store answers
;; =============================================================================

(defn- route-roots
  "The first segment of every route, as keywords, without `help`."
  [routes]
  (into #{}
        (comp (map (comp first tokens))
              (remove #{"help"})
              (map keyword))
        routes))

(defn- first-summary
  [bs]
  (some (fn [{:keys [summary description]}]
          (or summary (some-> description str/split-lines first)))
        bs))

(defn compose
  "SPECS, a tool's contributions {command spec}, with BRANCHES composed in.
   Returns {:specs :refusals}.

   HOST {:routes #{command} :next-handler h :guard g}. A branch on a root nobody
   contributed needs HOST to tell whether the host's own tree routes that root: it
   is then seeded with :next-handler, the re-entry that reaches the host's verb.
   With no :routes such a branch is refused, because a contribution of that name
   would otherwise replace a verb this cannot see."
  [specs branches tool {:keys [routes next-handler guard]}]
  (let [contributed (into #{} (map keyword) (keys specs))
        known       (route-roots routes)
        root-of     (comp first :path)
        whole?      (some (comp empty? :path) branches)
        no-host?    (fn [{:keys [path]}]
                      (and (seq path) (nil? routes) (not (contains? contributed (first path)))))
        {blocked true usable false} (group-by (comp boolean no-host?) branches)
        touched     (into #{} (keep root-of) usable)
        seeds       (if whole? known (filter known touched))
        tree        (merge (into {} (map (fn [r] [r next-handler])) seeds)
                           (into {} (map (fn [[cmd spec]] [(keyword cmd) (:handler spec)])) specs))
        {folded :tree refusals :refusals}
        (fold tree usable tool
              {:guard    guard
               :routes   (or routes #{})
               :declared (fn [root] (:verbs (get specs (name root))))})
        refused     (into #{} (map (juxt :kind :path :owner)) refusals)
        live        (remove #(contains? refused ((juxt :kind :path :owner) %)) usable)
        live-roots  (if (some (comp empty? :path) live)
                      (into (set (keys tree)) (keep root-of) live)
                      (into #{} (keep root-of) live))]
    {:refusals (into (vec refusals) (map #(refuse % :branch/no-host)) blocked)
     :specs
     (reduce
      (fn [acc root]
        (let [cmd  (name root)
              mine (filter #(= root (root-of %)) live)
              base (get specs cmd {:addon (:owner (or (first mine) (first live)))})]
          (assoc acc cmd
                 (cond-> (assoc base :handler (get folded root))
                   (seq (keep :params mine))
                   (update :params #(apply merge % (keep :params mine)))
                   (and (nil? (:summary base)) (nil? (:description base)) (first-summary mine))
                   (assoc :summary (first-summary mine))))))
      (or specs {})
      live-roots)}))
