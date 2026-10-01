(ns hive-addon.opaque.subprocess-test
  "The per-request deadline of the subprocess transport, against REAL kernels.

   The transport is the subject here, so the kernels are real OS processes
   (small `sh` scripts): one that answers, one that never answers, one that
   answers late once and on time afterwards, one that exits. A stub could not
   show what is at stake: that the process is gone, that its children are gone,
   and that no late answer reaches the next request."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-addon.opaque :as opaque]
            [hive-addon.opaque.schema :as os]
            [hive-addon.opaque.transport :as t]
            [hive-addon.opaque.transport.subprocess :as sub]
            [hive-addon.protocol :as proto]
            [malli.generator :as mg])
  (:import [clojure.lang ExceptionInfo]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Kernels
;; =============================================================================

(def ^:private echo-kernel
  "Answers every line with itself."
  "while IFS= read -r l; do printf '%s\\n' \"$l\"; done")

(def ^:private silent-kernel
  "Reads every line and never answers. Writes its blocking child's pid to $1."
  "while IFS= read -r l; do sleep 30 & echo $! > \"$1\"; wait; done")

(def ^:private answers-once-kernel
  "Answers the first line, then never answers again."
  (str "IFS= read -r l; printf '%s\\n' \"$l\"; "
       "while IFS= read -r l; do sleep 30 & wait; done"))

(def ^:private late-once-kernel
  "Answers every line with itself, but the very first line across ALL its runs
   (marked by the file $1) is answered one second late."
  (str "while IFS= read -r l; do "
       "if [ ! -e \"$1\" ]; then : > \"$1\"; sleep 1; fi; "
       "printf '%s\\n' \"$l\"; done"))

(def ^:private exits-after-one-kernel
  "Answers one line and exits 0."
  "IFS= read -r l; printf '%s\\n' \"$l\"")

(def ^:private wire-kernel
  "Speaks the opaque wire: initialize!, describe, health and shutdown! answer;
   tool \"echo\" answers and tool \"spin\" never does."
  (str/join
   "\n"
   ["while IFS= read -r l; do"
    "  case \"$l\" in"
    "    *':op :addon/initialize!'*) echo '{:op :addon/initialize! :result {:success? true :errors []}}' ;;"
    "    *':op :addon/describe'*) echo '{:op :addon/describe :result {:addon/id \"sh.kernel\" :addon/type :external :addon/capabilities #{:tools} :addon/tools [{:name \"spin\"} {:name \"echo\"}] :addon/excluded-tools #{} :addon/schema-extensions [] :addon/hooks {}}}' ;;"
    "    *':op :addon/health'*) echo '{:op :addon/health :result {:status :ok :details {}}}' ;;"
    "    *':op :addon/shutdown!'*) echo '{:op :addon/shutdown! :result nil}' ;;"
    "    *'\"spin\"'*) sleep 30 ;;"
    "    *':op :addon/tool'*) echo '{:op :addon/tool :result {:echoed true}}' ;;"
    "    *) echo '{:error \"unknown op\"}' ;;"
    "  esac"
    "done"]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- temp-path
  "A fresh path inside a new temp directory; the file itself does not exist."
  [name]
  (str (Files/createTempDirectory "opaque-deadline" (make-array FileAttribute 0))
       "/" name))

(defn- transport
  [script args opts]
  (sub/subprocess-transport (into ["sh" "-c" script "kernel"] args) opts))

(defn- timed
  "Run f; answer [value-or-throwable elapsed-ms]."
  [f]
  (let [t0 (System/nanoTime)
        v  (try (f) (catch Throwable e e))]
    [v (/ (- (System/nanoTime) t0) 1e6)]))

(defn- process-of [tr] (:process @(:state tr)))

(defn- pid-alive?
  [pid]
  (let [h (java.lang.ProcessHandle/of (long pid))]
    (boolean (and (.isPresent h) (.isAlive ^java.lang.ProcessHandle (.get h))))))

(defn- await-file
  "The trimmed contents of `path` once it is non-empty, waiting up to 2s."
  [path]
  (loop [n 0]
    (let [f (io/file path)
          s (when (.exists f) (str/trim (slurp f)))]
      (cond (seq s)  s
            (< n 100) (do (Thread/sleep 20) (recur (inc n)))
            :else    nil))))

(def ^:private tolerance-ms
  "How far past its deadline a missed request may return."
  1500)

;; =============================================================================
;; The transport
;; =============================================================================

(deftest a-kernel-that-answers-in-time-is-answered
  (let [tr (transport echo-kernel [] {:init-timeout-ms 5000 :request-timeout-ms 1000})]
    (try
      (t/-start! tr)
      (is (= "ping" (t/-request! tr "ping")))
      (is (= "pong" (t/-request! tr "pong")))
      (is (t/-alive? tr))
      (is (nil? (t/down-reason tr)))
      (finally (t/-stop! tr)))
    (testing "and a stop on request leaves no reason behind"
      (is (not (t/-alive? tr)))
      (is (nil? (t/down-reason tr))))))

(deftest a-silent-kernel-misses-the-deadline-and-is-stopped
  (let [pidfile (temp-path "child.pid")
        tr      (transport silent-kernel [pidfile] {:init-timeout-ms 300 :request-timeout-ms 300})]
    (try
      (t/-start! tr)
      (let [p       (process-of tr)
            [e ms]  (timed #(t/-request! tr "hello"))
            child   (some-> (await-file pidfile) parse-long)]
        (testing "the deadline fires, within tolerance"
          (is (instance? ExceptionInfo e))
          (is (<= 300 ms (+ 300 tolerance-ms)) (str "returned after " ms " ms")))
        (testing "and the error NAMES it: the first request is bound by the init deadline"
          (is (= (str "kernel did not answer within 300 ms (:opaque/init-timeout-ms);"
                      " it was stopped, and initialize! restarts it")
                 (ex-message e)))
          (is (= {:opaque/deadline-key :opaque/init-timeout-ms :opaque/deadline-ms 300}
                 (ex-data e))))
        (testing "the kernel process is gone, and so is the child it was blocked on"
          (is (not (.isAlive ^Process p)))
          (is (some? child) "the kernel never recorded its child's pid")
          (is (not (pid-alive? child))))
        (testing "the transport reports itself down, with the reason"
          (is (not (t/-alive? tr)))
          (is (= (ex-message e) (t/down-reason tr)))))
      (finally (t/-stop! tr)))))

(deftest the-request-deadline-applies-once-the-kernel-has-answered
  (let [tr (transport answers-once-kernel [] {:init-timeout-ms 5000 :request-timeout-ms 300})]
    (try
      (t/-start! tr)
      (is (= "first" (t/-request! tr "first")))
      (let [[e ms] (timed #(t/-request! tr "second"))]
        (is (instance? ExceptionInfo e))
        (is (<= 300 ms (+ 300 tolerance-ms))
            (str "bound by the 300 ms request deadline, not the 5000 ms init one; took " ms))
        (is (str/includes? (ex-message e) "within 300 ms (:opaque/request-timeout-ms)")))
      (finally (t/-stop! tr)))))

(deftest requests-after-a-miss-fail-fast-and-do-not-restart
  (let [marker (temp-path "late.marker")
        tr     (transport late-once-kernel [marker] {:init-timeout-ms 300 :request-timeout-ms 300})]
    (try
      (t/-start! tr)
      (let [[e _] (timed #(t/-request! tr "one"))
            [e2 ms] (timed #(t/-request! tr "again"))]
        (is (instance? ExceptionInfo e))
        (testing "a later request gets the SAME reason, at once"
          (is (instance? ExceptionInfo e2))
          (is (= (ex-message e) (ex-message e2)))
          (is (< ms 100) (str "took " ms " ms")))
        (testing "and no kernel was started to serve it"
          (is (nil? (process-of tr)))
          (is (not (t/-alive? tr)))))
      (finally (t/-stop! tr)))))

(deftest a-late-answer-never-reaches-the-next-request
  ;; The failure this whole design exists to prevent: a line protocol with no
  ;; request ids reads the late answer to "one" as the answer to "two".
  (let [marker (temp-path "late.marker")
        tr     (transport late-once-kernel [marker] {:init-timeout-ms 300 :request-timeout-ms 300})]
    (try
      (t/-start! tr)
      (let [first-process (process-of tr)
            [e _]         (timed #(t/-request! tr "one"))]
        (is (instance? ExceptionInfo e))
        ;; Past the moment the late answer to "one" would have been written.
        (Thread/sleep 1200)
        (t/-start! tr)
        (testing "-start! brought up a NEW process"
          (is (t/-alive? tr))
          (is (not (identical? first-process (process-of tr)))))
        (testing "and each request reads its own answer"
          (is (= "two" (t/-request! tr "two")))
          (is (= "three" (t/-request! tr "three"))))
        (is (nil? (t/down-reason tr)) "a restart clears the reason"))
      (finally (t/-stop! tr)))))

(deftest start-restarts-a-kernel-that-exited
  (let [tr (transport exits-after-one-kernel [] {:init-timeout-ms 5000 :request-timeout-ms 1000})]
    (try
      (t/-start! tr)
      (is (= "a" (t/-request! tr "a")))
      (.waitFor ^Process (process-of tr) 2 java.util.concurrent.TimeUnit/SECONDS)
      (testing "an exited kernel says how it exited"
        (is (not (t/-alive? tr)))
        (is (= "kernel process exited with status 0" (t/down-reason tr))))
      (t/-start! tr)
      (is (t/-alive? tr))
      (is (= "b" (t/-request! tr "b")))
      (finally (t/-stop! tr)))))

(deftest stop-ends-a-request-still-waiting-on-its-deadline
  ;; -stop! does not take the request lock, so shutdown! can end a hung call
  ;; long before its deadline would.
  (let [pidfile (temp-path "child.pid")
        tr      (transport silent-kernel [pidfile] {:init-timeout-ms 60000 :request-timeout-ms 60000})]
    (t/-start! tr)
    (let [pending (future (try (t/-request! tr "x") (catch Throwable e e)))]
      (await-file pidfile)
      (let [[_ ms] (timed #(t/-stop! tr))]
        (is (not= ::hung (deref pending 5000 ::hung)))
        (is (< ms 5000) (str "stop took " ms " ms"))
        (is (not (t/-alive? tr)))))))

;; =============================================================================
;; Through the proxy
;; =============================================================================

(defn- handler [proxy tool-name]
  (:handler (first (filter #(= tool-name (:name %)) (proto/tools proxy)))))

(deftest the-proxy-names-the-deadline-and-reports-why-it-is-down
  (let [spec  {:opaque/exec                "sh"
               :opaque/args                ["-c" wire-kernel "kernel"]
               :opaque/id                  "sh.kernel"
               :opaque/request-timeout-ms  300
               :opaque/init-timeout-ms     5000}
        proxy (opaque/subprocess-addon spec)]
    (try
      (is (:success? (proto/initialize! proxy {})))
      (is (= #{"spin" "echo"} (set (map :name (proto/tools proxy)))))
      (is (= {:echoed true} ((handler proxy "echo") {})))

      (let [[answer ms] (timed #((handler proxy "spin") {}))
            reason      (:error answer)]
        (testing "the tool call returns within tolerance of its deadline"
          (is (<= 300 ms (+ 300 tolerance-ms)) (str "took " ms " ms")))
        (testing "with an error that names the deadline, not a closed pipe"
          (is (string? reason))
          (is (str/includes? reason "within 300 ms (:opaque/request-timeout-ms)"))
          (is (not (str/includes? reason "closed the pipe"))))
        (testing "health is :down, and says why"
          (let [h (proto/health proxy)]
            (is (= :down (:status h)))
            (is (= reason (get-in h [:details :last-error])))))
        (testing "a tool call after the miss does NOT restart the kernel"
          (is (= {:error reason} ((handler proxy "echo") {})))
          (is (not (t/-alive? (:transport proxy))))))

      (testing "re-initialize restarts it cleanly"
        (is (:success? (proto/initialize! proxy {})))
        (is (= {:echoed true} ((handler proxy "echo") {})))
        (is (= :ok (:status (proto/health proxy)))))
      (finally (proto/shutdown! proxy)))))

;; =============================================================================
;; The spec keys
;; =============================================================================

(deftest the-spec-carries-the-deadlines
  (testing "positive integers are admitted"
    (is (os/validate os/OpaqueSpec {:opaque/exec "k"
                                    :opaque/request-timeout-ms 1
                                    :opaque/init-timeout-ms 60000})))
  (testing "anything else is refused"
    (doseq [bad [0 -5 1.5 "300" nil]]
      (is (not (os/validate os/OpaqueSpec {:opaque/exec "k" :opaque/request-timeout-ms bad}))
          (pr-str bad))
      (is (not (os/validate os/OpaqueSpec {:opaque/exec "k" :opaque/init-timeout-ms bad}))
          (pr-str bad))))
  (testing "subprocess-addon hands them to the transport, defaulting what is absent"
    (is (= {:request-timeout-ms 250 :init-timeout-ms sub/default-init-timeout-ms}
           (:opts (:transport (opaque/subprocess-addon {:opaque/exec "k"
                                                        :opaque/request-timeout-ms 250})))))
    (is (= {:request-timeout-ms sub/default-request-timeout-ms
            :init-timeout-ms    sub/default-init-timeout-ms}
           (:opts (:transport (opaque/subprocess-addon {:opaque/exec "k"}))))))
  (testing "over any spec the schema admits, the transport gets two positive
            deadlines: the spec's where it names one, the default where not"
    (let [specs (mg/sample os/OpaqueSpec {:size 40 :seed 11})]
      (is (some :opaque/request-timeout-ms specs) "the sample never exercised the key")
      (is (some (complement :opaque/request-timeout-ms) specs) "nor its absence")
      (doseq [spec specs]
        (let [opts (:opts (:transport (opaque/subprocess-addon spec)))]
          (is (os/validate [:map {:closed true}
                            [:request-timeout-ms os/TimeoutMs]
                            [:init-timeout-ms os/TimeoutMs]]
                           opts))
          (is (= (or (:opaque/request-timeout-ms spec) sub/default-request-timeout-ms)
                 (:request-timeout-ms opts)))
          (is (= (or (:opaque/init-timeout-ms spec) sub/default-init-timeout-ms)
                 (:init-timeout-ms opts))))))))
