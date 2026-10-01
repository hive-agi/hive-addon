(ns hive-addon.opaque.codec-test
  "Coverage for the opaque wire codec.

   The codec is the ONE namespace the host and a compiled proprietary kernel
   both load, so a defect here is a defect in every addon in the marketplace at
   once, and it shows up as a wrong ANSWER rather than as a crash. Two things
   are therefore proven rather than asserted by example:

   1. The ROUND TRIP is a law, over values drawn from the schema rather than
      from a fixture list. `encode` then `decode` must be the identity on every
      WireVal, because the proxy's whole contract is that what the kernel
      returned is what the host sees.

   2. `edn-safe`'s judgement agrees with the WireVal schema. Those are the same
      rule stated twice, once in code for the kernel (which cannot load malli)
      and once in malli for the host, and nothing but a test holds them
      together. A drift here is silent: the kernel would send a value the host's
      contract rejects."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.opaque.codec :as codec]
            [hive-addon.opaque.schema :as os]
            [hive-schemas.test :as hst]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; The round-trip law
;; =============================================================================

(hst/deftrifecta-from-schema encode-round-trips
  hive-addon.opaque.codec/encode
  {:in        [:cat os/WireVal]
   :out       :string
   :mutation  false
   :num-tests 200
   :rel       (fn [[v] line] (= v (codec/decode line)))})

(defspec every-wire-value-is-edn-safe-already 200
  ;; edn-safe is IDEMPOTENT on the vocabulary: a value the schema admits must
  ;; pass through untouched. Without this clause a projection that quietly
  ;; rewrote (say) every set into a vector would still round-trip and would
  ;; still satisfy every other property here.
  (prop/for-all [v (mg/generator os/WireVal)]
    (and (codec/safe? v) (= v (codec/edn-safe v)))))

(defspec edn-safe-always-lands-in-the-vocabulary 200
  ;; The other direction, and the one that matters for a config nobody vetted:
  ;; whatever goes in, what comes out is a WireVal. The generator deliberately
  ;; mixes UNSAFE values (a fn, an atom, a host object) into safe containers.
  (prop/for-all [v (gen/recursive-gen
                    (fn [inner]
                      (gen/one-of [(gen/vector inner 0 4)
                                   (gen/set inner {:max-elements 4})
                                   (gen/map (gen/one-of [gen/keyword gen/string-alphanumeric])
                                            inner
                                            {:max-elements 4})]))
                    (gen/one-of [gen/small-integer gen/string-alphanumeric gen/keyword
                                 gen/boolean (gen/return nil)
                                 (gen/return (fn [] :not-data))
                                 (gen/return (atom :not-data))
                                 (gen/return (Object.))]))]
    (let [projected (codec/edn-safe v)]
      (and (os/validate os/WireVal projected)
           ;; and it is stable: projecting an already-projected value changes
           ;; nothing, so a host may re-project defensively at any boundary
           (= projected (codec/edn-safe projected))))))

;; =============================================================================
;; What must NOT cross
;; =============================================================================

(defrecord ProbeRecord [a])

(deftest live-values-are-stripped-not-serialized
  (testing "a mount config's live dependencies are dropped, the rest survives"
    (let [config {:addon/id "acme.rank"
                  :addon/config {:tuning 0.5}
                  :mount/dependencies {"store" (atom :a-live-addon)}
                  :runtime/callback (fn [_] :nope)}
          out    (codec/edn-safe config)]
      (is (= {:addon/id "acme.rank" :addon/config {:tuning 0.5}
              :mount/dependencies {}}
             out)
          "an unsafe SCALAR value takes its key with it; a COLLECTION of unsafe
           values keeps its key and comes out empty")
      (is (os/validate os/WireVal out))))

  (testing "one bad leaf costs that leaf, never the map it sits in"
    ;; The regression this clause pins: gating a whole subtree on its worst leaf
    ;; loses data the kernel needs, and only a config that MIXES live objects
    ;; with real settings shows it. A fixture holding one or the other passes
    ;; under either rule.
    (is (= {:opts {:tuning 0.5 :retries 3}}
           (codec/edn-safe {:opts {:tuning 0.5 :cb (fn [_] :nope) :retries 3}}))))

  (testing "a record crosses as a plain map, and safe? says so"
    ;; EDN has no reader for a record, so it comes back as a map. safe? must
    ;; agree, or the law `safe? implies unchanged` would be false for records.
    (is (not (codec/safe? (->ProbeRecord 1))))
    (is (= {:a 1} (codec/edn-safe (->ProbeRecord 1))))
    (is (os/validate os/WireVal (codec/edn-safe (->ProbeRecord 1))))))

(deftest doubles-that-do-not-round-trip-are-refused
  (testing "NaN and the infinities are outside the vocabulary"
    ;; They print and they read back, but NaN is not equal to itself, so a
    ;; round-trip law over them would be false while nothing was wrong. They are
    ;; excluded from the vocabulary rather than special-cased in the law.
    (is (not (codec/safe? (/ 0.0 0.0))))
    (is (not (codec/safe? (/ 1.0 0.0))))
    (is (not (codec/safe? (/ -1.0 0.0))))
    (is (nil? (codec/edn-safe (/ 0.0 0.0)))))
  (testing "and ordinary doubles, zero included, are not caught by that rule"
    ;; 0.0 is the trap: an infinity is the only NON-ZERO value equal to half of
    ;; itself, and an implementation that forgets the qualifier rejects zero.
    (is (codec/safe? 0.0))
    (is (codec/safe? -0.0))
    (is (codec/safe? 2.5))
    (is (codec/safe? 1.0e308))))

;; =============================================================================
;; Framing
;; =============================================================================

(deftest encoding-is-a-function-of-the-value-alone
  (testing "a host that bound *print-length* cannot truncate a wire line"
    ;; This is the failure the pinned printer exists to prevent, and it is the
    ;; worst kind: the truncated line is still valid EDN, so it decodes cleanly
    ;; into a WRONG value and nothing reports an error.
    (let [v {:op :addon/tool :result (vec (range 40))}]
      (binding [*print-length* 3 *print-level* 1]
        (is (= v (codec/decode (codec/encode v))))))))

(deftest a-line-that-is-not-content-decodes-to-nil
  (is (nil? (codec/decode nil)) "EOF")
  (is (nil? (codec/decode "")) "an empty line")
  (is (nil? (codec/decode "   \n")) "a blank line")
  (is (nil? (codec/decode 42)) "a non-string, which a pipe can hand you"))

(deftest a-malformed-line-throws-here-on-purpose
  (testing "decode does not catch, so that this namespace needs no catch class"
    ;; Catching would need a reader conditional, and a reader conditional is
    ;; what keeps a namespace OUT of the three-host portable stratum. The two
    ;; callers are boundaries that already own a rescue; see the docstring.
    (is (thrown? Exception (codec/decode "{:op :addon/health")))))

(deftest an-unknown-tagged-literal-degrades-to-its-value
  (testing "a kernel built against a newer vocabulary does not kill the host"
    (is (= {:op :addon/health :args {:since 5}}
           (codec/decode "{:op :addon/health :args #acme/future {:since 5}}")))))

;; =============================================================================
;; The projections both sides agree on
;; =============================================================================

(deftest tool-summary-drops-exactly-the-handler
  (let [tool {:name "rank" :description "d" :inputSchema {:type "object"}
              :handler (fn [_] :called)}]
    (is (= {:name "rank" :description "d" :inputSchema {:type "object"}}
           (codec/tool-summary tool)))
    (is (os/validate os/ToolSummary (codec/tool-summary tool)))))

(deftest hook-summary-distinguishes-a-fn-from-a-table
  (testing "a fn is ANNOUNCED, since the proxy must install a callback for it"
    (is (= {:kind :fn} (codec/hook-summary (fn [_] :x))))
    (is (os/validate os/HookSummary (codec/hook-summary (fn [_] :x)))))
  (testing "and data TRAVELS, so a hook that is a lookup table stays one"
    (is (= {:kind :data :value {:a 1}} (codec/hook-summary {:a 1})))
    (is (os/validate os/HookSummary (codec/hook-summary {:a 1}))))
  (testing "a data hook holding a fn is announced as a fn, not half-sent"
    (is (= {:kind :fn} (codec/hook-summary {:a (fn [_] :x)})))))

(deftest normalize-args-gives-the-kernel-one-shape
  (testing "MCP hands string keys and an in-process caller hands keywords"
    (is (= {:x 1 :y 2} (codec/normalize-args {"x" 1 :y 2}))))
  (testing "and a non-map is an empty arg map, not a throw"
    (is (= {} (codec/normalize-args nil)))
    (is (= {} (codec/normalize-args "not a map")))))

(deftest the-op-vocabulary-is-closed
  (testing "every op is a qualified keyword in the :addon namespace"
    (is (every? qualified-keyword? codec/ops))
    (is (every? #(= "addon" (namespace %)) codec/ops)))
  (testing "and the schema enum and the code set are the same set"
    ;; Two statements of the vocabulary, one for the host and one for the
    ;; kernel. This is the assertion that keeps them one.
    (is (= codec/ops (set (rest os/Op))))))

(deftest messages-validate-against-their-schemas
  (is (os/validate os/Request (codec/request :addon/health)))
  (is (os/validate os/Request (codec/request :addon/tool {:tool "rank" :params {:x 1}})))
  (is (os/validate os/Response (codec/ok :addon/health {:status :ok})))
  (is (os/validate os/Response (codec/error "unreadable request line")))
  (is (os/validate os/Response (codec/error :addon/tool "no such tool")))
  (testing "a nil args is OMITTED rather than sent as an explicit nil"
    (is (= {:op :addon/health} (codec/request :addon/health nil)))))

(deftest error-and-result-read-a-response
  (is (codec/error? (codec/error :addon/tool "boom")))
  (is (not (codec/error? (codec/ok :addon/tool {:v 1}))))
  (is (= {:v 1} (codec/result (codec/ok :addon/tool {:v 1}))))
  (testing "a nil RESULT and a FAILURE are distinguishable, which is why
            error? exists rather than a nil check"
    (is (nil? (codec/result (codec/ok :addon/shutdown! nil))))
    (is (not (codec/error? (codec/ok :addon/shutdown! nil))))
    (is (nil? (codec/result (codec/error :addon/shutdown! "boom"))))
    (is (codec/error? (codec/error :addon/shutdown! "boom")))))
