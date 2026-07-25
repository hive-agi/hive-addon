(ns hive-addon.cli.response
  "The tool-result shape a CLI handler returns.

   A host may enrich these — attaching recovery hints or pending agent
   instructions — by passing its own builders to `hive-addon.cli/make-handler`.")

;; SPDX-License-Identifier: MIT

(defn text
  "A successful result carrying TEXT."
  [text]
  {:type "text" :text text})

(defn error
  "A failed result carrying MESSAGE."
  [message]
  {:type "text" :text message :isError true})

(defn error?
  "True iff RESPONSE is an error result."
  [response]
  (true? (:isError response)))
