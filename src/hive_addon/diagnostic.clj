(ns hive-addon.diagnostic
  "Pure diagnostics for addon lifecycle reports."
  (:require [hive-help.core :as help]
            [malli.core :as m]))

(def mount-guidance
  {:entitlement "Ask the operator to check the entitlement policy. Do not bypass the gate."
   :resolved "Check the constructor namespace and function in the mount manifest and confirm their dependencies are on the classpath."
   :config "Check the addon configuration and required dependency bindings."
   :failed "Check that the constructor completes and returns an IAddon instance."
   :registered "Check the host registration result before attempting initialization."
   :initialized "Inspect initialization and the addon health report before retrying."
   :skipped "Inspect the mount plan and resolve its prerequisite failures."})

(def Diagnostic
  "The recovery record attached to a failed outcome."
  [:map {:closed true}
   [:code :qualified-keyword]
   [:retryable :boolean]
   [:actions [:vector [:map {:closed false} [:tool :string]]]]
   [:message :string]])

(defn mount-outcome
  "Attach recovery guidance to a failed mount result."
  [result]
  (if (:success? result)
    result
    (let [phase (:phase result)]
      (assoc result :diagnostic
             {:code (keyword "addon.mount" (name phase))
              :retryable false
              :actions []
              :message (help/join-lines
                        (str "Addon " (pr-str (:addon/id result))
                             " did not mount successfully at phase " (name phase) ".")
                        (str "HINT: " (get mount-guidance phase
                                           "Inspect the mount report before retrying.")))}))))

;; A FAILED result must carry a :phase (named in the message); a successful
;; one passes through untouched.
(m/=> mount-outcome [:=> [:cat [:or [:map {:closed false} [:success? [:= true]]]
                                    [:map {:closed false} [:phase [:or :keyword :string]]]]]
                     [:map {:closed false} [:diagnostic {:optional true} Diagnostic]]])

(defn missing-addon
  "Describe an addon absent from the supplied mount specifications."
  [ids]
  {:code :addon/not-mounted
   :retryable false
   :actions [{:tool "hot" :arguments {:command "list"}}]
   :message (help/expected-message
             {:param ":addon/id" :expected "an addon in the current mount specifications"
              :actual (vec (sort ids))
              :hint "Run hive hot list. If the addon is absent, mount its project with hive hot inject using an absolute path, then verify its host dependency is included at startup. Reload cannot mount an absent addon."})})

(m/=> missing-addon [:=> [:cat [:or [:set :string] [:sequential :string]]] Diagnostic])
