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
   proprietary binary's diagnostics belong in the host's log anyway.

   Every request is bounded by a DEADLINE, and requests are serialized: the
   line protocol has no request ids, so one exchange at a time is what keeps
   answers paired with their requests. See `subprocess-transport` for what a
   missed deadline does."
  (:require [clojure.java.io :as io]
            [hive-addon.opaque.transport :as t])
  (:import [java.io BufferedReader Writer]
           [java.lang ProcessBuilder ProcessBuilder$Redirect ProcessHandle]
           [java.util.concurrent ExecutionException TimeUnit]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def default-request-timeout-ms
  "Deadline, in ms, for one request to a kernel that has already answered once.
   Overridden per kernel by :opaque/request-timeout-ms."
  30000)

(def default-init-timeout-ms
  "Deadline, in ms, for the FIRST request after the kernel process starts. That
   request also pays the process start and load, and in the proxy's lifecycle
   it is always :addon/initialize!. Overridden by :opaque/init-timeout-ms."
  60000)

(def ^:private stop-grace-ms
  "How long -stop! lets a kernel exit on EOF before destroying it."
  2000)

;; =============================================================================
;; The process
;; =============================================================================

(defn- spawn!
  [argv]
  (let [pb (ProcessBuilder. ^java.util.List (vec argv))]
    (.redirectError pb ProcessBuilder$Redirect/INHERIT)
    (let [p (.start pb)]
      {:process   p
       :writer    (io/writer (.getOutputStream p))
       :reader    (io/reader (.getInputStream p))
       :answered? false})))

(defn- descendants-of
  "Every live descendant of `process`, snapshotted now. Empty when it cannot be
   read."
  [^Process process]
  (try (vec (iterator-seq (.iterator (.descendants (.toHandle process)))))
       (catch Throwable _ [])))

(defn- terminate!
  "Stop one kernel process and return nil: close its stdin, give it `grace-ms`
   to exit on EOF, then destroy it and every descendant it had forcibly, and
   reap it. Never throws."
  [{:keys [^Process process ^Writer writer]} grace-ms]
  (let [family (descendants-of process)]
    (try (.close writer) (catch Throwable _ nil))
    (when (pos? grace-ms)
      (try (.waitFor process (long grace-ms) TimeUnit/MILLISECONDS)
           (catch Throwable _ nil)))
    (doseq [^ProcessHandle h family]
      (try (.destroyForcibly h) (catch Throwable _ nil)))
    (when (.isAlive process)
      (try (.destroyForcibly process) (catch Throwable _ nil)))
    (try (.waitFor process 2 TimeUnit/SECONDS) (catch Throwable _ nil))
    nil))

(defn- running?
  "True when `s` holds a process that is still running."
  [s]
  (let [^Process p (:process s)]
    (boolean (and p (.isAlive p)))))

(defn- exit-reason
  "Why a process that is no longer running stopped, or nil while it runs."
  [^Process process]
  (when (and process (not (.isAlive process)))
    (str "kernel process exited with status " (.exitValue process))))

;; =============================================================================
;; One bounded exchange
;; =============================================================================

(defn- deadline-for
  "[spec-key ms] of the deadline that applies to the next request on `running`."
  [opts running]
  (if (:answered? running)
    [:opaque/request-timeout-ms (:request-timeout-ms opts)]
    [:opaque/init-timeout-ms (:init-timeout-ms opts)]))

(defn- missed-deadline
  "The reason recorded when a request misses its deadline."
  [spec-key ms]
  (str "kernel did not answer within " ms " ms (" spec-key
       "); it was stopped, and initialize! restarts it"))

(defn- exchange!
  "Write `line` to the running kernel and read one answer line within the
   deadline. On a miss: stop the process, record the reason in `state`, and
   throw it. Must be called holding the transport's lock."
  [state opts running line]
  (let [{:keys [^Process process ^Writer writer ^BufferedReader reader]} running
        [spec-key ms] (deadline-for opts running)
        pending (future
                  (.write writer (str line "\n"))
                  (.flush writer)
                  (.readLine reader))
        answer  (try (deref pending (long ms) ::missed)
                     (catch ExecutionException e
                       (throw (or (.getCause e) e))))
        owns?   (fn [s] (identical? process (:process s)))]
    (if (identical? ::missed answer)
      (let [reason (missed-deadline spec-key ms)]
        (terminate! running 0)
        (future-cancel pending)
        (swap! state (fn [s] (if (owns? s) {:down-reason reason} s)))
        (throw (ex-info reason {:opaque/deadline-key spec-key
                                :opaque/deadline-ms  ms})))
      (do (when-not (:answered? running)
            (swap! state (fn [s] (if (owns? s) (assoc s :answered? true) s))))
          answer))))

;; =============================================================================
;; The transport
;; =============================================================================

(defrecord SubprocessTransport [argv opts state]
  t/ITransport

  (-start! [this]
    (locking (:state this)
      (let [s @(:state this)]
        (when-not (running? s)
          (when (:process s) (terminate! s 0))
          (reset! (:state this) (spawn! (:argv this))))))
    this)

  (-request! [this line]
    (locking (:state this)
      (let [s @(:state this)]
        (cond
          (:process s)     (exchange! (:state this) (:opts this) s line)
          (:down-reason s) (throw (ex-info (:down-reason s) {:opaque/down? true}))
          :else            nil))))

  (-alive? [this]
    (running? @(:state this)))

  ;; Deliberately NOT under the lock: stopping must be able to end a request
  ;; that is still waiting on its deadline.
  (-stop! [this]
    (let [s @(:state this)]
      (when (:process s)
        (terminate! s stop-grace-ms)))
    (reset! (:state this) nil)
    nil)

  t/IDiagnosable

  (-down-reason [this]
    (try
      (let [s @(:state this)]
        (or (:down-reason s) (exit-reason (:process s))))
      (catch Throwable _ nil))))

(defn subprocess-transport
  "An ITransport that runs `argv` as a subprocess on first -start!. `argv` is
   the whole command vector (executable first), so a kernel that needs flags,
   a licence file path or a data directory gets them without this namespace
   knowing what any of them mean.

   `opts` may carry :request-timeout-ms and :init-timeout-ms (positive ms); a
   nil or absent one takes `default-request-timeout-ms` / `default-init-timeout-ms`.
   The init deadline bounds the first request after each start, the request
   deadline every later one.

   A request that misses its deadline throws ex-info whose message names the
   deadline, after stopping the kernel (stdin closed, then the process and its
   descendants destroyed forcibly). The process is never reused: the line
   protocol is desynced by then, and a late answer would be read as the next
   request's. Until the next -start! the transport is not alive, -down-reason
   answers the reason, and every -request! throws it WITHOUT restarting the
   kernel. -start! restarts it, and OpaqueAddon/initialize! is what calls
   -start!, so a restarted kernel always receives :addon/initialize! before it
   serves another tool. -start! likewise restarts a kernel that exited."
  ([argv] (subprocess-transport argv nil))
  ([argv opts]
   (->SubprocessTransport
    (vec argv)
    {:request-timeout-ms (or (:request-timeout-ms opts) default-request-timeout-ms)
     :init-timeout-ms    (or (:init-timeout-ms opts) default-init-timeout-ms)}
    (atom nil))))
