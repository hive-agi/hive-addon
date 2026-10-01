(ns portable.oracle-opaque
  "Differential oracle for the OPAQUE KERNEL TIER: hive-addon.opaque.codec and
   hive-addon.opaque.serve, the two namespaces a compiled proprietary addon
   loads.

   A JVM + cljw leg, and cljw is the host that matters here: the kernel IS a
   cljw build. Where portable.oracle asks whether the hot-reload core behaves
   the same on three runtimes, this asks the sharper question the marketplace
   depends on, which is whether a vendor's addon answers the SAME BYTES once it
   is compiled. A divergence in this diff is a customer receiving different
   results from the binary than from the source it was built from.

   cljrs is absent for two independent reasons, either of which alone would be
   enough: `serve` dispatches through a protocol defined in another namespace,
   which cljrs cannot do (the same limitation that puts the IMountDriver leg in
   oracle_driver.cljc), and cljrs is not installed on this machine, so a claim
   about it would be an assertion rather than a measurement.

   Everything emitted is canonicalized. Sets are sorted into vectors and maps
   are read key by key: set iteration order and map print order are
   host-dependent and are not behaviour, so leaving them raw produces diffs that
   say nothing, which trains the reader to ignore the diff."
  (:require [hive-addon.opaque.codec :as codec]
            [hive-addon.opaque.serve :as serve]
            [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn emit [label v] (println (str label " | " (pr-str v))))

(defn sorted-set-of
  "Canonicalize a set to a sorted vector of its printed elements."
  [s]
  (vec (sort (map pr-str s))))

;; =============================================================================
;; The vocabulary
;; =============================================================================

(emit "ops"            (sorted-set-of codec/ops))
(emit "op?/known"      (codec/op? :addon/tool))
(emit "op?/unknown"    (codec/op? :addon/teleport))

;; =============================================================================
;; safe? and edn-safe: the judgement the host restates in malli
;; =============================================================================

(def wire-values
  "One value per branch of the vocabulary, in a FIXED order (a vector, not a
   set), so the emitted lines are comparable positionally."
  [nil "s" 0 -7 42 0.0 2.5 true false :k 'sym
   [] [1 [2 {:a :b}]] #{} {} {:a 1 "b" [2 3]}])

(emit "safe?/vocabulary"  (mapv codec/safe? wire-values))
(emit "edn-safe/identity" (mapv (fn [v] (= v (codec/edn-safe v))) wire-values))

;; NaN and the infinities are excluded because they do not round-trip through
;; equality. Computed rather than written as literals: ##NaN is not readable on
;; every host, and a literal would make this leg a test of the READER.
(def nan (/ 0.0 0.0))
(def pos-inf (/ 1.0 0.0))
(def neg-inf (/ -1.0 0.0))

(emit "safe?/nan"      (codec/safe? nan))
(emit "safe?/+inf"     (codec/safe? pos-inf))
(emit "safe?/-inf"     (codec/safe? neg-inf))
(emit "safe?/zero"     (codec/safe? 0.0))
(emit "edn-safe/nan"   (codec/edn-safe nan))

;; The leaf-drop rule. This is the observation a whole-subtree gate would fail.
(emit "edn-safe/leaf-drop"
      (codec/edn-safe {:opts {:tuning 0.5 :cb (fn [_] :nope) :retries 3}}))
(emit "edn-safe/live-deps"
      (codec/edn-safe {:addon/id "k" :mount/dependencies {"store" (atom :live)}}))
(emit "edn-safe/unsafe-scalar" (codec/edn-safe (fn [_] :nope)))

;; =============================================================================
;; Framing
;; =============================================================================

(emit "encode/request"    (codec/encode (codec/request :addon/health)))
(emit "encode/with-args"  (codec/encode (codec/request :addon/tool {:tool "rank" :params {:x 2}})))
(emit "encode/ok"         (codec/encode (codec/ok :addon/health {:status :ok})))
(emit "encode/error"      (codec/encode (codec/error :addon/tool "no such tool")))

(emit "round-trip"
      (mapv (fn [v] (= v (codec/decode (codec/encode v)))) wire-values))

(emit "decode/nil"        (codec/decode nil))
(emit "decode/empty"      (codec/decode ""))
(emit "decode/blank"      (codec/decode "   "))
(emit "decode/non-string" (codec/decode 42))
(emit "decode/tagged"     (codec/decode "{:op :addon/health :args #acme/future {:since 5}}"))

;; Printer independence: a host that bound *print-length* must not be able to
;; truncate a line into valid-but-wrong EDN.
(emit "encode/print-length-pinned"
      (binding [*print-length* 3 *print-level* 1]
        (let [v {:op :addon/tool :result (vec (range 12))}]
          (= v (codec/decode (codec/encode v))))))

(emit "normalize-args/strings"  (codec/normalize-args {"x" 1 :y 2}))
(emit "normalize-args/non-map"  (codec/normalize-args "not a map"))
(emit "tool-summary"            (codec/tool-summary {:name "rank" :description "d"
                                                     :handler (fn [_] :called)}))
(emit "hook-summary/fn"         (codec/hook-summary (fn [_] :x)))
(emit "hook-summary/data"       (codec/hook-summary {:a 1}))
(emit "health-report/up"        (codec/health-report true {:vendor "acme"}))
(emit "health-report/down"      (codec/health-report false nil))
(emit "init-result/ok"          (codec/init-result true))
(emit "init-result/failed"      (codec/init-result false ["boom"]))

;; =============================================================================
;; serve: the kernel side
;; =============================================================================

(defrecord ProbeKernel [state]
  proto/IAddon
  (addon-id [_] "probe.kernel")
  (addon-type [_] :external)
  (capabilities [_] #{:tools :health-reporting})
  (initialize! [this config]
    (reset! (:state this) {:up? true :config config})
    {:success? true :errors []})
  (shutdown! [this] (swap! (:state this) assoc :up? false) nil)
  (tools [_]
    [{:name "double"
      :description "doubles x"
      :inputSchema {:type "object"}
      :handler (fn [{:keys [x]}] {:result (* 2 x)})}])
  (schema-extensions [_] [{:probe/score {:db/valueType :db.type/long}}])
  (health [this] {:status (if (:up? @(:state this)) :ok :down) :details {}})
  (excluded-tools [_] #{"double_basic"})
  (hooks [_] {:probe/fn (fn [n] {:got n}) :probe/data {:a 1}}))

(def kernel (->ProbeKernel (atom {})))

(let [d (serve/describe kernel)]
  (emit "describe/id"           (:addon/id d))
  (emit "describe/type"         (:addon/type d))
  (emit "describe/capabilities" (sorted-set-of (:addon/capabilities d)))
  (emit "describe/tools"        (:addon/tools d))
  (emit "describe/excluded"     (sorted-set-of (:addon/excluded-tools d)))
  (emit "describe/schema-ext"   (:addon/schema-extensions d))
  (emit "describe/hook-fn"      (get-in d [:addon/hooks :probe/fn]))
  (emit "describe/hook-data"    (get-in d [:addon/hooks :probe/data])))

;; A legacy kernel that omits the optional methods must default them exactly as
;; the host registry does, on every host. This is the observation that catches a
;; per-host difference in how a missing protocol method is reported.
(defrecord LegacyKernel []
  proto/IAddon
  (addon-id [_] "probe.legacy")
  (addon-type [_] :external)
  (capabilities [_] #{})
  (initialize! [_ _] {:success? true :errors []})
  (shutdown! [_] nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok}))

(let [d (serve/describe (->LegacyKernel))]
  (emit "legacy/excluded" (sorted-set-of (:addon/excluded-tools d)))
  (emit "legacy/hooks"    (:addon/hooks d)))

(emit "respond/initialize"
      (serve/respond kernel (codec/encode (codec/request :addon/initialize! {:tuning 0.5}))))
(emit "respond/tool"
      (serve/respond kernel (codec/encode (codec/request :addon/tool {:tool "double" :params {"x" 21}}))))
(emit "respond/hook-fn"
      (serve/respond kernel (codec/encode (codec/request :addon/hook {:key :probe/fn :args [7]}))))
(emit "respond/hook-data"
      (serve/respond kernel (codec/encode (codec/request :addon/hook {:key :probe/data}))))
(emit "respond/health"
      (serve/respond kernel (codec/encode (codec/request :addon/health))))
(emit "respond/unreadable"  (serve/respond kernel "{:op :addon/health"))
(emit "respond/not-a-map"   (serve/respond kernel "42"))
(emit "respond/unknown-op"  (serve/respond kernel (codec/encode {:op :addon/teleport})))
(emit "respond/no-such-tool"
      (serve/respond kernel (codec/encode (codec/request :addon/tool {:tool "nope"}))))
(emit "respond/no-such-hook"
      (serve/respond kernel (codec/encode (codec/request :addon/hook {:key :nope/x}))))
(emit "handle-line/is-one-line"
      (let [l (serve/handle-line kernel (codec/encode (codec/request :addon/health)))]
        [(string? l) (nil? (re-find #"\n" l))]))
(emit "respond/shutdown"
      (serve/respond kernel (codec/encode (codec/request :addon/shutdown!))))
(emit "respond/health-after-shutdown"
      (serve/respond kernel (codec/encode (codec/request :addon/health))))

(println "ORACLE-OPAQUE-END")
