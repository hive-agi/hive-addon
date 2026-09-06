(ns hive-addon.mount.maturity
  "How finished a MountSpec is, and what a host does about it. Pure.

   Sibling of hive-addon.mount.entitlement: entitlement answers MAY this mount,
   maturity answers HOW FINISHED is it. They are separate axes (a proprietary
   addon can be :stable and a FOSS one :experimental), so neither reads the
   other's field.

   The field is `:addon/maturity`, NOT `:addon/status`. hive-store already owns
   `:addon/status` for a different claim, whether a customer's token resolves
   the coordinate today, and a preview coordinate of stable code is a real
   combination. One keyword answering two questions is the drift this module
   exists to remove.

   This namespace is the ONE place the absent-key default lives. A consumer
   that spells `(:addon/maturity spec :experimental)` itself has forked the
   default, and the fork is invisible until the two disagree."
  (:require [hive-addon.schema :as s]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def default-maturity
  "What an undeclared :addon/maturity means.

   :experimental, never :stable. A manifest that never made a maturity claim
   must not be read as having made the strongest one. It is the same trap the
   :foss default of :addon/trust-class sprang, where an absent key silently
   granted the permissive reading to every addon in the fleet."
  :experimental)

(def levels
  "Every legal maturity, WEAKEST FIRST. The rendering order for any surface
   that groups addons by maturity, and the source of the ordering itself.

   The order is information the ADT does not carry: a sum type states the
   variant set, not that :beta outranks :experimental. So the vector is
   declared here and reconciled against the type below."
  [:dormant :experimental :beta :stable])

(def ^:private variant-drift
  "Variants the type and the ordering disagree about, as {:missing .. :extra ..},
   or nil when they agree."
  (let [declared (set levels)
        variants (:variants s/AddonMaturity)
        missing (remove declared variants)
        extra (remove variants levels)]
    (when (or (seq missing) (seq extra))
      {:missing (vec missing) :extra (vec extra)})))

(when variant-drift
  (throw (ex-info (str "hive-addon.mount.maturity/levels disagrees with "
                       "hive-addon.schema/AddonMaturity: every variant needs a "
                       "rank, and a rank with no variant is dead ordering.")
                  variant-drift)))

(def ^:private rank
  "Maturity as a total order, derived from `levels` so adding a variant means
   placing it in one vector rather than editing a parallel map."
  (into {} (map-indexed (fn [i level] [level i])) levels))

(defn maturity
  "This spec's maturity claim, defaulted."
  [spec]
  (get spec :addon/maturity default-maturity))

(defn at-least?
  "True when `spec` is at least as mature as `floor`.

   The predicate a host policy is written in terms of: a production host asks
   for :beta and up, a curated catalog for :stable, and neither has to
   enumerate what that excludes."
  [floor spec]
  (>= (get rank (maturity spec) -1)
      (get rank floor 0)))

(defn dormant?
  "True when this spec is declared out of development."
  [spec]
  (= :dormant (maturity spec)))

(defn declared?
  "True when the manifest states its maturity rather than inheriting the
   default.

   The fleet gate is written against this, not against `maturity`: every value
   including the default reads the same through `maturity`, so only this can
   tell a deliberate :experimental from an unanswered one."
  [spec]
  (contains? spec :addon/maturity))

(defn coerce
  "A bare maturity keyword as an AddonMaturity ADT value, or nil when the
   keyword is not a variant. The seam for anything that must round-trip a
   maturity through the type rather than trust a raw keyword."
  [kw]
  (s/->addon-maturity kw))

(def Maturity
  "Re-exported so a caller can validate a maturity without reaching into the
   schema layer."
  s/Maturity)
