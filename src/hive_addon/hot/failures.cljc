(ns hive-addon.hot.failures
  "Addons a reload FAILED to load, and the refusal that keeps them out of the
   next one.

   A reload whose namespace pass stops partway, or whose constructor namespace
   will not load, leaves the image half old and half new: protocols re-minted
   next to records and registries that were not. Remounting the addon on top
   of that hands it half-new code. Retrying the same reload on every `git live`
   pass re-mints protocols into the broken environment each time and spreads
   the half-loaded state further (card 20261118113047-64765ca8, axiom
   20260824152125-5b78b916).

   So a reload is all-or-nothing per addon. An addon whose code failed to load
   keeps its PREVIOUS instance mounted, untouched, and is marked :reload/failed
   here with the error. Every later reload whose slice reaches a marked addon is
   refused before any namespace is loaded, and the report says why. The mark is
   lifted by a restart (this state lives in the image), by a reload that asks
   for it (:retry-failed? or :force?), or by a reload that mounts the addon's
   new code successfully.

   Pure apart from the one atom; the clock is the caller's."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; {addon-id FailureMark}. defonce: a reload of this namespace must not forget
;; which addons are half-loaded.
(defonce ^:private marks (atom {}))

(defn failures
  "{addon-id mark} of every addon currently marked :reload/failed."
  []
  @marks)

(defn failed?
  "Is `id` marked :reload/failed?"
  [id]
  (contains? @marks id))

(defn clear!
  "Lift the mark from `ids` (every mark when called with no ids). Returns the
   remaining marks."
  ([] (reset! marks {}))
  ([ids] (apply swap! marks dissoc ids)))

(defn blocked
  "Pure. {seed [marked-id ...]} for the seeds whose reload closure (`closure`
   seed -> #{id ...}, the seed included) reaches an addon in `marked` (a set or
   map keyed by id). Seeds with no marked addon in reach are absent."
  [seeds closure marked]
  (into (sorted-map)
        (keep (fn [seed]
                (when-let [hits (seq (sort (filter #(contains? marked %) (closure seed))))]
                  [seed (vec hits)])))
        seeds))

(defn refusal-line
  "The plain sentence a refused seed reports."
  [seed hits marks]
  (let [why (fn [id]
              (let [{:reload/keys [phase error]} (get marks id)]
                (str id " (" (some-> phase name) ": " error ")")))]
    (str seed ": reload REFUSED. "
         (if (= [seed] hits)
           "Its last reload failed to load its code"
           "Its reload reaches addon(s) whose last reload failed to load their code")
         ": " (str/join "; " (map why hits))
         ". The previous instance keeps serving. Restart the host, or reload with"
         " force (retry-failed) once the code is fixed.")))

(defn failed-line
  "The plain sentence a reload that marks `ids` failed reports."
  [ids {:reload/keys [phase error failed-ns]}]
  (str (str/join ", " ids) ": reload FAILED"
       (when phase (str " at " (name phase)))
       (when failed-ns (str " (namespace " failed-ns ")"))
       ": " error
       ". The previous instance keeps serving and " (if (next ids) "they are" "it is")
       " marked :reload/failed; further reloads are refused until a restart or a"
       " reload with force (retry-failed)."))

(defn next-marks
  "Pure. The marks after `report` (a RemountReport) ran at `now`.

   Ids under :hot/reload-failures are marked (attempts counted up from any
   earlier mark); ids whose NEW instance mounted (a :mounted row with
   :success? true) are cleared. Everything else keeps its mark."
  [marks report now]
  (let [failed  (:hot/reload-failures report)
        mounted (into #{} (comp (filter :success?) (map :addon/id)) (:mounted report))
        trigger (:hot/trigger report)]
    (as-> marks m
      (apply dissoc m (remove #(contains? failed %) mounted))
      (reduce-kv (fn [acc id info]
                   (assoc acc id
                          (cond-> (merge info
                                         {:reload/status   :reload/failed
                                          :reload/at       now
                                          :reload/attempts (inc (get-in marks [id :reload/attempts] 0))})
                            trigger (assoc :reload/trigger trigger))))
                 m
                 (or failed {})))))

(defn record!
  "Fold `report` into the marks at `now` (epoch ms). Returns the marks."
  [report now]
  (swap! marks next-marks report now))
