(ns hive-addon.opaque.opaque-facets
  "Shared fixture for the opaque tier's facet tests: one small vendor kernel
   (a real IAddon, so it is a faithful port implementation, not a stub of a
   concretion) and helpers that project wire answers to plain EDN."
  (:require [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defrecord MiniKernel [state]
  proto/IAddon
  (addon-id [_] "mini.kernel")
  (addon-type [_] :external)
  (capabilities [_] #{:tools})
  (initialize! [this config]
    (reset! (:state this) {:up? true})
    {:success? true :errors [] :metadata {:keys (count config)}})
  (shutdown! [this] (swap! (:state this) assoc :up? false) nil)
  (tools [_]
    [{:name "double"
      :description "doubles x"
      :inputSchema {:type "object" :properties {"x" {:type "number"}}}
      :handler (fn [{:keys [x]}] {:result (* 2 x)})}])
  (schema-extensions [_] [])
  (health [this] {:status (if (:up? @(:state this)) :ok :down)})
  (excluded-tools [_] #{})
  (hooks [_] {:mini/table {:a 1}}))

(defn kernel [] (->MiniKernel (atom {})))

(def spec
  {:opaque/exec "/opt/mini/kernel"
   :opaque/id "mini.kernel"
   :opaque/capabilities #{:tools}})
