(ns hive-addon.hot.report
  "Pure verdicts every hot-reload report is built from — computed from what
   actually happened, never asserted by construction.

   A report that says :teardown/data-preserved? true because the skeleton was
   written that way, or :ok? false with no word about whether the addon is
   still running, answers the question the operator did not ask. These
   functions derive each claim from the outcomes the ports returned:

     data-preserved?   from the teardown outcomes that actually released
     remount-outcome   which failed ids were put back, which are DOWN
     released-dirs     which watched dirs no surviving addon still needs

   Portable stratum: no reader conditionals, no host API, no effects.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; data-preserved? — a claim about releases, so it is computed from them
;; =============================================================================

(defn released?
  "Did this teardown outcome release anything?"
  [outcome]
  (boolean (seq (:torn-down outcome))))

(defn data-preserved?
  "True when every teardown outcome that RELEASED something claims
   :teardown/data-preserved? true. An outcome that tore nothing down released
   nothing and cannot have deleted anything, so it does not count against the
   verdict; one that released without making the claim does — an unclaimed
   release is not evidence of preservation."
  [outcomes]
  (every? #(true? (:teardown/data-preserved? %))
          (filter released? outcomes)))

;; =============================================================================
;; remount-outcome — what is running after a remount that may have failed
;; =============================================================================

(defn remount-outcome
  "Partition the IDS a remount tore down by what the MountReport says became of
   them:

     :hot/restored   the new instance failed and the PREVIOUS one is live again
     :hot/down       neither the new nor the previous instance is live

   An id the report never attempted is down: it was torn down and nothing put
   anything back. :hot/restored? is present only when something failed, and is
   true exactly when nothing is down."
  [ids mount-report]
  (let [by-id    (into {} (map (juxt :addon/id identity)) (:mounted mount-report))
        up?      #(:success? (by-id %))
        restored (into [] (filter #(true? (:restored? (by-id %)))) ids)
        failed   (into [] (remove up?) ids)
        down     (into [] (remove (set restored)) failed)]
    (cond-> {:hot/restored restored
             :hot/down     down}
      (seq failed) (assoc :hot/restored? (empty? down)))))

;; =============================================================================
;; released-dirs — plug-out may only stop watching what nobody else needs
;; =============================================================================

(defn released-dirs
  "The EJECTED addons' source dirs that no REMAINING addon still lives under,
   sorted. A dir two addons share stays watched while either is mounted."
  [ejected-dirs remaining-dirs]
  (vec (sort (remove (set remaining-dirs) (distinct ejected-dirs)))))
