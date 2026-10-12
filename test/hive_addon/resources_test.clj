(ns hive-addon.resources-test
  "The optional IAddon resource contribution contract."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-addon.protocol :as proto]
            [hive-addon.resources :as resource]
            [hive-addon.schema :as schema]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn resource-contract [descriptor]
  (schema/validate :addon/resource-def descriptor))

(def ^:private valid {:uri "workflow://one" :name "One"
                     :mimeType "application/edn" :handler (fn [_] "{}")})

(deftrifecta resource-descriptor-contract
  #'resource-contract
  {:golden-path "test/golden/hive-addon/resource-descriptor.edn"
   :cases {:valid valid :missing-handler (dissoc valid :handler)
           :missing-uri (dissoc valid :uri) :empty-name (assoc valid :name "")}
   :gen (gen/elements [valid (dissoc valid :handler) (dissoc valid :uri)])
   :pred boolean?
   :mutations [["accept-all" (fn [_] true)]]
   :assert (fn []
             (is (resource-contract valid))
             (is (not (resource-contract (dissoc valid :handler))))
             (is (schema/validate :addon/resources [valid]))
             (is (not (schema/validate :addon/resources [(dissoc valid :uri)]))))})

(defn- resource-addon []
  (reify proto/IAddon
    (addon-id [_] "demo.resources")
    (addon-type [_] :native)
    (capabilities [_] #{:resources})
    (initialize! [_ _] {:success? true})
    (shutdown! [_] nil)
    (tools [_] [])
    (schema-extensions [_] [])
    (health [_] {:status :ok})
    resource/IAddonResources
    (list-resources [_] [valid])
    (read-resources [_ uri] ((:handler valid) uri))))

(defn- legacy-addon []
  (reify proto/IAddon
    (addon-id [_] "legacy.resources")
    (addon-type [_] :native)
    (capabilities [_] #{})
    (initialize! [_ _] {:success? true})
    (shutdown! [_] nil)
    (tools [_] [])
    (schema-extensions [_] [])
    (health [_] {:status :ok})))

(defn- invalid-addon []
  (reify proto/IAddon
    (addon-id [_] "invalid.resources")
    (addon-type [_] :native)
    (capabilities [_] #{:resources})
    (initialize! [_ _] {:success? true})
    (shutdown! [_] nil)
    (tools [_] [])
    (schema-extensions [_] [])
    (health [_] {:status :ok})
    resource/IAddonResources
    (list-resources [_] [(dissoc valid :uri)])
    (read-resources [_ _] (throw (ex-info "Invalid handler invoked" {})))))

(defn resource-read [addon]
  (resource/resources addon))

(deftrifecta resource-protocol-dispatch
  #'resource-read
  {:golden-path "test/golden/hive-addon/resource-protocol.edn"
   :cases {:declared (resource-addon) :legacy (legacy-addon)}
   :xf (fn [rs] (mapv #(select-keys % [:uri :name :mimeType]) rs))
   :gen (gen/return (resource-addon))
   :pred (fn [rs] (and (= 1 (count rs)) (schema/validate :addon/resources rs)))
   :mutations [["no-resources" (fn [_] [])]]
   :assert (fn []
             (is (= "workflow://one" (:uri (first (resource-read (resource-addon))))))
             (is (= [] (resource-read (legacy-addon))))
             (is (:ok (schema/validate-addon (legacy-addon))))
             (is (:ok (schema/validate-addon (resource-addon))))
             (is (= "{}" (resource/read-resource (resource-addon) "workflow://one")))
             (is (nil? (resource/read-resource (resource-addon) "workflow://missing")))
             (is (= :invalid-resources
                    (:addon/error (ex-data (try (resource/resources (invalid-addon))
                                                 (catch clojure.lang.ExceptionInfo e e)))))))})

(deftest iaddon-method-set-is-frozen
  (is (= #{:addon-id :addon-type :capabilities :initialize! :shutdown! :tools
           :schema-extensions :health :excluded-tools :hooks}
         (set (keys (:sigs proto/IAddon))))))
