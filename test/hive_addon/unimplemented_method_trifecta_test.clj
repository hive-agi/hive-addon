(ns hive-addon.unimplemented-method-trifecta-test
  "Schema, property and mutation checks for `unimplemented-method?`, the
   portable test both the registry and the opaque server use to let an addon
   omit an optional IAddon method.

   The cases are the measured phrasings each host uses (JVM defrecord, reify
   and extend, cljw, cljrs, cljs) plus near misses that must NOT match. The
   law reads the expected verdict from this table, never from the subject.
   ClojureCLR's NotImplementedException arm is proven by the CLR differential
   leg (test/portable/run_clojureclr.sh), which this JVM suite cannot reach."
  (:require [clojure.test.check.generators :as gen]
            [hive-schemas.test :as hst]
            [hive-test.properties :refer [defprop-metamorphic]]
            [hive-addon.protocol :as proto]))

(def ^:private table
  "message -> expected verdict."
  {"Method p.Partial.b()Ljava/lang/Object; is abstract"                          true
   "Receiver class x$reify__8710 does not define or inherit an implementation of the resolved method" true
   "No implementation of method: :b of protocol: #'p/IThing found for class: p.Partial" true
   "No implementation of method 'b' on protocol 'IThing' for type 'Partial'"  true
   "runtime error: No implementation of protocol IThing for type Partial"     true
   "No protocol method IThing.b defined for type object"                      true
   "nothing implements this method"                                           true
   "Divide by zero"                                                           false
   "connection refused"                                                       false
   "the method is implemented abstractly elsewhere"                           false})

(def ^:private throwables
  (mapv #(ex-info % {}) (keys table)))

(def ^:private Thrown (into [:enum] throwables))

(defn- law [t out] (= (get table (ex-message t)) out))

(hst/deftrifecta-from-schema unimplemented-method-contract
  #'proto/unimplemented-method?
  {:in Thrown
   :out :boolean
   :rel law
   :contract true
   :num-tests 60
   :seed 0
   :n-cases 10})

(defn- shout [t] (ex-info (.toUpperCase ^String (ex-message t)) {}))

(defprop-metamorphic matching-ignores-case
  proto/unimplemented-method?
  shout
  =
  (gen/elements throwables)
  {:num-tests 60})
