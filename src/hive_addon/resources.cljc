(ns hive-addon.resources
  "Optional MCP resource contribution contract, independent of IAddon."
  (:require [hive-addon.schema :as schema]))

(defprotocol IAddonResources
  (list-resources [this]
    "Return dynamically enumerated MCP resource descriptors with :uri, :name,
     optional :description/:mimeType, and a JVM-local :handler (fn [uri] ...).")
  (read-resources [this uri]
    "Read the descriptor selected by the host. Called only for an advertised URI."))

(defn resources
  "Host accessor. Legacy addons have no resources. Invalid descriptors are
   refused before they can be advertised or invoked."
  [addon]
  (if (satisfies? IAddonResources addon)
    (let [descriptors (list-resources addon)]
      (if (schema/validate :addon/resources descriptors)
        descriptors
        (throw (ex-info "Invalid addon resource descriptors"
                        {:addon/error :invalid-resources}))))
    []))

(defn read-resource
  "Read only a URI advertised by this addon; never invoke an undeclared handler."
  [addon uri]
  (when (some #(= uri (:uri %)) (resources addon))
    (read-resources addon uri)))
