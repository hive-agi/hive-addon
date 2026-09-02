(ns hive-addon.opaque.transport.inproc
  "In-process ITransport backed by a (fn [request-line] -> response-line).

   No subprocess, but a FAITHFUL transport: it moves the same lines the
   subprocess moves, so a test that drives an addon through it is exercising the
   real wire rather than a stub of it. Two uses:

     - drive a kernel that is on this classpath, before it is compiled opaque,
       so the same addon can be proven in-process and then shipped as a binary;
     - inject a recording line function in a test, which is the only way to
       assert the exact BYTES the proxy sends."
  (:require [hive-addon.opaque.transport :as t]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defrecord FnTransport [line-fn started]
  t/ITransport
  (-start!   [this] (reset! (:started this) true) this)
  (-request! [this line] (when @(:started this) ((:line-fn this) line)))
  (-alive?   [this] (boolean @(:started this)))
  (-stop!    [this] (reset! (:started this) false) nil))

(defn fn-transport
  "An ITransport whose -request! is `line-fn`, a (request-line -> response-line).
   It answers nil before -start! and after -stop!, so a caller that skips the
   lifecycle sees the same nil a dead pipe gives it."
  [line-fn]
  (->FnTransport line-fn (atom false)))
