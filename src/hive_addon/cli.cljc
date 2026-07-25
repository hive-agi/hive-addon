(ns hive-addon.cli
  "CLI-style subcommand dispatch for a consolidated tool.

   `make-handler` turns a handler tree into a single fn dispatching on the
   :command parameter, supporting n-depth paths (\"status list\") alongside
   single-word commands."
  (:require [hive-addon.cli.response :as response]
            [hive-addon.cli.tree :as tree]))

;; SPDX-License-Identifier: MIT

(defn- coercer
  "The coercion fn for COERCE-SCHEMA, or nil when no schema is given."
  [coerce-schema]
  (when coerce-schema
    #?(:clj (requiring-resolve 'hive-dsl.coerce/coerce-map)
       :cljs nil)))

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
                     declared types before dispatch (see hive-dsl.coerce)
     :error-fn       builds an error result; defaults to
                     `hive-addon.cli.response/error`, and a host may pass its
                     own enriched builder"
  ([handlers] (make-handler handlers {}))
  ([handlers {:keys [coerce-schema error-fn]}]
   (let [error-fn (or error-fn response/error)
         coerce-fn (coercer coerce-schema)]
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
