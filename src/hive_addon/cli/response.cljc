(ns hive-addon.cli.response
  "The tool-result shape a CLI handler returns.

   A host may enrich these — attaching recovery hints or pending agent
   instructions — by passing its own builders to `hive-addon.cli/make-handler`.")

;; SPDX-License-Identifier: MIT

(defn text
  "A successful result carrying CONTENT.

   The parameter is not called `text`: on cljrs a defn parameter sharing the
   FUNCTION'S OWN name does not shadow the self-reference, so the body binds the
   function object and this returned {:text #<Fn text>}. A parameter named after
   any OTHER var shadows correctly, and so does a `let` — it is specifically the
   self-name collision that breaks."
  [content]
  {:type "text" :text content})

(defn error
  "A failed result carrying MESSAGE."
  [message]
  {:type "text" :text message :isError true})

(defn error?
  "True iff RESPONSE is an error result."
  [response]
  (true? (:isError response)))
