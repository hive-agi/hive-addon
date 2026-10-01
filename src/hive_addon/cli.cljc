(ns hive-addon.cli
  "CLI-style subcommand dispatch for a consolidated tool.

   `make-handler` turns a handler tree into a single fn dispatching on the
   :command parameter, supporting n-depth paths (\"status list\") alongside
   single-word commands."
  (:require [hive-addon.cli.response :as response]
            [hive-addon.cli.tree :as tree]))

;; SPDX-License-Identifier: MIT

(defn- unknown-command
  [error-fn command handlers]
  (error-fn (str "Unknown command: " command ". Valid: " (keys handlers))))

(defn- dispatch
  "Invoke HANDLER with PARAMS, coercing first when COERCE-FN is present."
  [handler params coerce-fn coerce-schema error-fn]
  (if coerce-fn
    (let [coerced (coerce-fn coerce-schema params)]
      (if (:ok coerced)
        (handler (:ok coerced))
        (error-fn (str "Parameter error: " (:message coerced)))))
    (handler params)))

(defn make-handler
  "A handler fn dispatching on the :command parameter of its argument map.

   HANDLERS maps keyword segments to handler fns or to nested trees; a tree
   may carry :_handler as its default. \"help\" at the root renders the
   command list.

   OPTS:
     :coerce-schema  {field [type-spec]} — string params are coerced to the
                     declared types before dispatch
     :coerce-fn      (fn [schema params] -> Result) performing that coercion.
                     REQUIRED whenever :coerce-schema is given.
                     `hive-dsl.coerce/coerce-map` is the usual argument.
     :error-fn       builds an error result; defaults to
                     `hive-addon.cli.response/error`, and a host may pass its
                     own enriched builder

   Coercion arrives as an INJECTED FN rather than being resolved here. This
   namespace previously did `#?(:clj (requiring-resolve 'hive-dsl.coerce/...))`,
   which is wrong twice over: `:clj` does not select the JVM (cljw presents it,
   cljrs presents :rust, so on cljrs the form vanished and coercion silently
   became a no-op), and `hive-dsl.coerce` pulls in clojure.data.json — a
   dependency this leaf library declines to acquire on every consumer's behalf
   for a feature most of them do not use.

   Declaring :coerce-schema without :coerce-fn THROWS rather than dispatching
   uncoerced. Silently handing a handler the raw string params it asked to have
   coerced is the failure this refusal exists to prevent, and wiring-time is
   where it can still be fixed cheaply."
  ([handlers] (make-handler handlers {}))
  ([handlers {:keys [coerce-schema coerce-fn error-fn]}]
   (when (and coerce-schema (not (ifn? coerce-fn)))
     (throw (ex-info (str "hive-addon.cli/make-handler: :coerce-schema was given without a"
                          " callable :coerce-fn. Pass hive-dsl.coerce/coerce-map (or your own"
                          " (fn [schema params] -> Result)), or drop :coerce-schema.")
                     {:cli/error :missing-coerce-fn
                      :coerce-schema coerce-schema
                      :coerce-fn coerce-fn})))
   (let [error-fn (or error-fn response/error)]
     (fn [{:keys [command] :as params}]
       (let [path (tree/parse-command (tree/normalize-command command))]
         (cond
           (nil? path)
           (unknown-command error-fn command handlers)

           (= [:help] path)
           (response/text (tree/format-help handlers))

           :else
           (if-let [handler (:handler (tree/resolve-handler handlers path))]
             (dispatch handler params coerce-fn coerce-schema error-fn)
             (unknown-command error-fn command handlers))))))))
