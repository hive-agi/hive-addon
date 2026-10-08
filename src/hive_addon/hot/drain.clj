(ns hive-addon.hot.drain
  "Drain an addon's calls in flight before it is plugged OUT.

   A host wraps every addon handler with `tracked`, so each call is admitted and
   counted through an IDrainGate. `drain!` closes the gate for the ids about to
   go (new calls are refused with ex-data {:error :hot/draining}), then waits,
   bounded by :drain-ms, until the calls already admitted have finished.
   `drain-verdict` reads the outcome; `open!` admits calls again.

   `drain-gate` builds the default adapter over ONE process-wide counter map, so
   the host's dispatch wrap and eject! observe the same calls."
  (:require [hive-addon.hot.port :as hport]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Adapter: counters in an atom
;; =============================================================================

(defrecord AtomDrainGate [state]
  hport/IDrainGate
  (-enter! [_ id]
    (let [[old new] (swap-vals! state
                                (fn [m] (if (get-in m [id :closed?])
                                          m
                                          (update-in m [id :in-flight] (fnil inc 0)))))]
      (not= (get-in old [id :in-flight]) (get-in new [id :in-flight]))))
  (-leave! [_ id]
    (swap! state update-in [id :in-flight] (fn [n] (max 0 (dec (or n 0)))))
    nil)
  (-close! [_ id]
    (swap! state assoc-in [id :closed?] true)
    nil)
  (-in-flight [_ id]
    (get-in @state [id :in-flight] 0))
  (-open! [_ id]
    (swap! state (fn [m] (if (pos? (get-in m [id :in-flight] 0))
                           (assoc-in m [id :closed?] false)
                           (dissoc m id))))
    nil))

(defn atom-drain-gate
  "A fresh IDrainGate over its own counters."
  []
  (->AtomDrainGate (atom {})))

(defonce ^:private process-calls (atom {}))

(defn drain-gate
  "The IDrainGate over the process-wide counters every host wrap and eject! share."
  []
  (->AtomDrainGate process-calls))

;; =============================================================================
;; Dispatch wrap
;; =============================================================================

(defn draining-message
  "Pure. What a call refused by a closed gate is told."
  [id]
  (str "addon " id " is being unmounted; the call was not run"))

(defn tracked
  "HANDLER as a call to addon ID admitted through GATE (default `drain-gate`).
   An admitted call is counted until it returns or throws. A call arriving while
   ID drains is not run: it throws ex-info with {:error :hot/draining :addon/id ID}."
  ([id handler] (tracked (drain-gate) id handler))
  ([gate id handler]
   (fn tracked-handler [& args]
     (if (hport/-enter! gate id)
       (try (apply handler args)
            (finally (hport/-leave! gate id)))
       (throw (ex-info (draining-message id) {:error :hot/draining :addon/id id}))))))

(defn draining?
  "Pure. Is EX the refusal `tracked` throws for a call to a draining addon?"
  [ex]
  (= :hot/draining (:error (ex-data ex))))

(defn dispatch-handler
  "HANDLER of addon ID as a host dispatches it, so every IAddon can be drained
   on unmount without code of its own: INNER (fn [id handler] -> handler), the
   host's own wrap, is applied first; the result is counted through GATE
   (default `drain-gate`); a call refused while ID drains is answered by
   (REFUSED message id) instead of thrown. Any other failure still throws.

   opts: {:inner fn? :refused fn? :gate IDrainGate?}. Without :refused the
   refusal is thrown as `tracked` throws it."
  [id handler & [{:keys [inner refused gate]}]]
  (let [h       (if inner (inner id handler) handler)
        counted (tracked (or gate (drain-gate)) id h)]
    (if refused
      (fn dispatched-handler [& args]
        (try (apply counted args)
             (catch clojure.lang.ExceptionInfo e
               (if (draining? e) (refused (ex-message e) id) (throw e)))))
      counted)))

;; =============================================================================
;; Drain: close, then wait bounded
;; =============================================================================

(def default-drain-ms
  "How long `drain!` waits for calls in flight when :drain-ms is not given."
  30000)

(def default-poll-ms
  "How often `drain!` re-reads the counters."
  25)

(defn in-flight
  "{id n} for the IDS that still have calls in flight under GATE."
  [gate ids]
  (into {} (keep (fn [id] (let [n (hport/-in-flight gate id)]
                            (when (pos? n) [id n]))))
        ids))

(defn drain!
  "Close GATE for IDS, then wait until none of them has a call in flight or
   :drain-ms has passed. The gate stays CLOSED either way; `open!` re-opens it.

   opts: {:drain-ms ms :poll-ms ms :now (fn [] ms) :sleep! (fn [ms])}
   Returns a DrainReport:
     {:hot/draining [id] :hot/in-flight {id n} :hot/waited-ms ms :hot/drain-ms ms}
   :hot/in-flight is what was still running when the wait ended."
  [gate ids & [{:keys [drain-ms poll-ms now sleep!]}]]
  (let [ids      (vec (distinct ids))
        drain-ms (or drain-ms default-drain-ms)
        poll-ms  (max 1 (or poll-ms default-poll-ms))
        now      (or now #(System/currentTimeMillis))
        sleep!   (or sleep! #(Thread/sleep (long %)))
        start    (now)]
    (run! #(hport/-close! gate %) ids)
    (loop []
      (let [busy   (in-flight gate ids)
            waited (- (now) start)]
        (if (or (empty? busy) (>= waited drain-ms))
          {:hot/draining  ids
           :hot/in-flight busy
           :hot/waited-ms waited
           :hot/drain-ms  drain-ms}
          (do (sleep! (min poll-ms (- drain-ms waited)))
              (recur)))))))

(defn open!
  "Admit calls to IDS through GATE again."
  [gate ids]
  (run! #(hport/-open! gate %) ids)
  nil)

(defn drain-verdict
  "Pure. How a plug OUT reads a DrainReport: :drained when nothing is in flight,
   :forced when calls remain but FORCE? was given, otherwise :busy."
  [report force?]
  (cond
    (empty? (:hot/in-flight report)) :drained
    force?                           :forced
    :else                            :busy))

(defn busy-message
  "Pure. The refusal a :busy drain answers with."
  [report]
  (str "calls still in flight after " (:hot/waited-ms report) " ms: "
       (pr-str (into (sorted-map) (:hot/in-flight report)))
       "; retry, raise :drain-ms, or pass :force? true"))
