(ns hive-addon.wire-test
  "One fold, used by every report that crosses a serializer. These tests pin the
   two choices that are not obvious: a function renders as a STABLE marker rather
   than its identity, and a set renders in a deterministic order. Both exist so
   that two reports of the same state compare equal, which is how a report is
   actually read."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-addon.wire :as wire]))

;; SPDX-License-Identifier: MIT

(deftest scalars-pass-through-untouched
  (is (= nil (wire/json-safe nil)))
  (is (= "s" (wire/json-safe "s")))
  (is (= true (wire/json-safe true)))
  (is (= 42 (wire/json-safe 42)))
  (is (= :k (wire/json-safe :k))))

(deftest a-function-renders-as-a-stable-marker
  (let [a (wire/json-safe (fn [] 1))
        b (wire/json-safe (fn [] 2))]
    (is (= "#fn" a))
    (is (= a b)
        "printing a function yields an identity hash, so the same report would
         differ from itself between two calls and defeat any diff of it")))

(deftest a-set-renders-in-a-deterministic-order
  (testing "JSON has no set, and an undefined order makes two equal reports
            compare unequal"
    (is (= (wire/json-safe #{:b :a :c})
           (wire/json-safe #{:c :b :a})))
    (is (vector? (wire/json-safe #{:a})))))

(deftest symbols-and-unknown-objects-become-strings
  (is (= "foo/bar" (wire/json-safe 'foo/bar)))
  (is (string? (wire/json-safe (java.util.Date.)))))

(deftest map-keys-a-json-writer-cannot-take-are-printed
  (let [folded (wire/json-safe {'sym 1 :kw 2 "s" 3})]
    (is (= #{"sym" :kw "s"} (set (keys folded))))
    (is (= 1 (get folded "sym")))))

(deftest the-fold-is-recursive-and-its-output-actually-serializes
  (let [live {:fns [(fn [] 1)]
              :nested {:set #{'a 'b} :date (java.util.Date.)}
              :ok [1 "two" :three nil true]}]
    (is (thrown? Exception (json/write-str live))
        "precondition: this is exactly the shape that used to kill a report")
    (is (string? (json/write-str (wire/json-safe live))))))
