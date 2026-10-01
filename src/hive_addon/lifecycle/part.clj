(ns hive-addon.lifecycle.part
  "Parts: resources an addon owns that can be released while the addon stays
   mounted, and reopened on the next use.

   A part is opened by its first `acquire!`, touched by every use, and closed by
   a sweep once it has been idle for its :idle-ms with nothing using it. Closing
   releases the resource (a cache, an index, a connection pool, a model); the
   part itself stays registered and opens again when next acquired.

     (def index (part/part {:id :carto/index :idle-ms 600000
                            :open  (fn [] (load-index))
                            :close (fn [idx] (release idx))}))
     (part/register-part! \"hive.carto\" index)
     (part/with-part [idx index] (query idx q))

   Closing never deletes data: :close releases what :open acquired."
  (:require [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- system-now [] (System/currentTimeMillis))

(defrecord Part [id idle-ms open close clock state])

(defn part
  "A closed part. OPTS:
     :id       keyword, unique within its owner
     :idle-ms  idle time before a sweep may close it
     :open     (fn [] resource)
     :close    (fn [resource]), optional
     :clock    (fn [] ms), optional, for tests"
  [{:keys [id idle-ms open close clock]}]
  {:pre [(keyword? id) (pos-int? idle-ms) (ifn? open)]}
  (->Part id idle-ms open close (or clock system-now)
          (atom {:open? false :resource nil :last-used-ms nil :in-flight 0
                 :opens 0 :closes 0 :last-error nil})))

(defn open?
  [p]
  (:open? @(:state p)))

(defn acquire!
  "The part's resource, opening it first when closed. Touches the part.
   Throws what :open throws; the part stays closed in that case."
  [p]
  (let [now ((:clock p))]
    (locking p
      (when-not (:open? @(:state p))
        (let [res ((:open p))]
          (swap! (:state p) assoc :open? true :resource res :last-error nil)
          (swap! (:state p) update :opens inc)))
      (swap! (:state p) assoc :last-used-ms now)
      (:resource @(:state p)))))

(defn begin-use!
  "Acquire P and count one use in flight. Pair with end-use!."
  [p]
  (locking p
    (let [res (acquire! p)]
      (swap! (:state p) update :in-flight inc)
      res)))

(defn end-use!
  [p]
  (swap! (:state p) (fn [st] (-> st
                                 (update :in-flight #(max 0 (dec %)))
                                 (assoc :last-used-ms ((:clock p)))))))

(defmacro with-part
  "Bind SYM to P's resource for BODY, holding P open for its duration."
  [[sym p] & body]
  `(let [p# ~p
         ~sym (begin-use! p#)]
     (try ~@body
          (finally (end-use! p#)))))

(defn close!
  "Close P now unless a use is in flight. Returns :closed, :already-closed or
   :in-flight. A :close that throws is recorded; the part still counts as closed."
  [p]
  (locking p
    (let [{:keys [open? resource in-flight]} @(:state p)]
      (cond
        (not open?)       :already-closed
        (pos? in-flight)  :in-flight
        :else
        (let [res (when-let [c (:close p)] (r/try-effect (c resource)))]
          (swap! (:state p) assoc :open? false :resource nil
                 :last-error (when (r/err? res) (:message res)))
          (swap! (:state p) update :closes inc)
          :closed)))))

(defn idle?
  "True when P is open, unused, and untouched for its :idle-ms as of NOW."
  [p now]
  (let [{:keys [open? in-flight last-used-ms]} @(:state p)]
    (and open?
         (zero? in-flight)
         (some? last-used-ms)
         (>= (- now last-used-ms) (:idle-ms p)))))

(defn status
  "P as data."
  [p]
  (-> @(:state p)
      (dissoc :resource)
      (assoc :id (:id p) :idle-ms (:idle-ms p))))

;; =============================================================================
;; Registry, by owner
;; =============================================================================

(defonce ^:private registry (atom {}))

(defn register-part!
  "Register P under OWNER (an addon id). A part already registered under the same
   id is closed and replaced. Returns P."
  [owner p]
  (let [[old _] (swap-vals! registry assoc-in [owner (:id p)] p)]
    (when-let [prev (get-in old [owner (:id p)])]
      (when-not (identical? prev p)
        (close! prev))))
  p)

(defn parts-of
  "{part-id Part} registered under OWNER."
  [owner]
  (get @registry owner {}))

(defn owners [] (vec (keys @registry)))

(defn close-owner!
  "Close every part of OWNER and forget them. Returns the ids closed now."
  [owner]
  (let [[old _] (swap-vals! registry dissoc owner)]
    (into [] (keep (fn [[id p]] (when (= :closed (close! p)) id)))
          (get old owner))))

(defn sweep!
  "Close every idle part as of NOW. Returns {owner [part-id ...]} for the parts
   closed."
  [now]
  (into {}
        (keep (fn [[owner parts]]
                (let [closed (into [] (keep (fn [[id p]]
                                              (when (and (idle? p now) (= :closed (close! p)))
                                                id)))
                                   parts)]
                  (when (seq closed) [owner closed]))))
        @registry))

(defn statuses
  "{owner [part-status ...]}."
  []
  (into {} (map (fn [[owner parts]] [owner (mapv status (vals parts))])) @registry))

(defn reset-registry!
  "Close and forget every part. For tests."
  []
  (doseq [owner (owners)] (close-owner! owner))
  nil)
