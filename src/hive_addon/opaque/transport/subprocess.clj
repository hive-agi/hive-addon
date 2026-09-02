(ns hive-addon.opaque.transport.subprocess
  "Effectful ITransport: a long-lived OS process running the opaque kernel
   binary, spoken to over line-oriented EDN on its stdin and stdout.

   This is the transport a marketplace artifact actually mounts through. The
   process is LONG-LIVED on purpose: a cljw binary costs roughly 36ms to start,
   which is invisible once per mount and ruinous once per tool call.

   JVM-only (.clj, not .cljc). It is the one namespace in the opaque subsystem
   that names host classes, which is precisely why the seam above it is a
   protocol: nothing else has to care that this exists.

   The kernel's stderr is INHERITED rather than piped. A piped stderr that
   nobody drains fills its buffer and deadlocks the kernel mid-call, and a
   proprietary binary's diagnostics belong in the host's log anyway."
  (:require [clojure.java.io :as io]
            [hive-addon.opaque.transport :as t])
  (:import [java.io BufferedReader Writer]
           [java.lang ProcessBuilder ProcessBuilder$Redirect]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- spawn!
  [argv]
  (let [pb (ProcessBuilder. ^java.util.List (vec argv))]
    (.redirectError pb ProcessBuilder$Redirect/INHERIT)
    (let [p (.start pb)]
      {:process p
       :writer  (io/writer (.getOutputStream p))
       :reader  (io/reader (.getInputStream p))})))

(defrecord SubprocessTransport [argv state]
  t/ITransport

  (-start! [this]
    (when-not @(:state this)
      (reset! (:state this) (spawn! (:argv this))))
    this)

  (-request! [this line]
    (when-let [{:keys [writer reader]} @(:state this)]
      (.write ^Writer writer (str line "\n"))
      (.flush ^Writer writer)
      (.readLine ^BufferedReader reader)))

  (-alive? [this]
    (boolean (some-> @(:state this) :process (.isAlive))))

  (-stop! [this]
    (when-let [{:keys [^Process process ^Writer writer]} @(:state this)]
      ;; Close stdin FIRST: a kernel whose loop reads to EOF exits on its own,
      ;; and a clean exit lets it release whatever it holds. destroyForcibly is
      ;; the fallback for one that does not, not the first move.
      (try (.close writer) (catch Throwable _ nil))
      (try (.waitFor process 2 java.util.concurrent.TimeUnit/SECONDS)
           (catch Throwable _ nil))
      (when (.isAlive process)
        (.destroyForcibly process)))
    (reset! (:state this) nil)
    nil))

(defn subprocess-transport
  "An ITransport that runs `argv` as a subprocess on first -start!. `argv` is
   the whole command vector (executable first), so a kernel that needs flags,
   a licence file path or a data directory gets them without this namespace
   knowing what any of them mean."
  [argv]
  (->SubprocessTransport (vec argv) (atom nil)))
