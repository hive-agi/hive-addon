(ns hive-addon.opaque.serve-test
  "Facets for the kernel side of the opaque wire: `respond` over every op and
   the three failure lines a kernel must answer instead of dying, and
   `optional`'s defaulting of methods a legacy addon does not implement."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.opaque.codec :as codec]
            [hive-addon.opaque.opaque-facets :as fx]
            [hive-addon.opaque.serve :as serve]
            [hive-addon.protocol :as proto]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn respond-to
  "serve/respond over a fresh kernel for one request LINE."
  [line]
  (serve/respond (fx/kernel) line))

(defn- line [op & [args]] (codec/encode (codec/request op args)))

(tri/deftrifecta respond-lines
  hive-addon.opaque.serve-test/respond-to
  {:golden-path "test/golden/hive-addon/opaque-serve-respond.edn"
   :cases {:describe (line :addon/describe)
           :health (line :addon/health)
           :tool (line :addon/tool {:tool "double" :params {:x 21}})
           :no-such-tool (line :addon/tool {:tool "nope"})
           :hook-data (line :addon/hook {:key :mini/table})
           :no-such-hook (line :addon/hook {:key :mini/none})
           :unknown-op (codec/encode {:op :addon/teleport})
           :unreadable "{:op"
           :not-a-map "[1 2 3]"}
   :mutations [["throws-on-garbage"
                (fn [l] (if (= "{:op" l) (throw (ex-info "dead kernel" {})) (respond-to l)))]
               ["no-op-echo"
                (fn [l] (dissoc (respond-to l) :op))]]})

(defrecord Legacy []
  proto/IAddon
  (addon-id [_] "legacy")
  (addon-type [_] :native)
  (capabilities [_] #{})
  (initialize! [_ _] {:success? true :errors []})
  (shutdown! [_] nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok}))

(deftest optional-defaults-only-a-missing-method
  (is (= #{} (serve/optional (->Legacy) proto/excluded-tools #{})))
  (is (= {:mini/table {:a 1}} (serve/optional (fx/kernel) proto/hooks {})))
  (is (thrown? Exception
               (serve/optional (fx/kernel) (fn [_] (throw (ex-info "broken" {}))) {}))
      "a broken method must not masquerade as an absent one"))
