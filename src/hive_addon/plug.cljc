(ns hive-addon.plug
  "Resolve ordered iaddon.edn layers (lowest precedence first) into the selected
   plug set: merge → validate → lint → profile → drop-disabled → capability-select."
  (:require [hive-addon.plug.schema :as schema]
            [hive-addon.plug.merge :as mrg]
            [hive-addon.plug.lint :as lint]
            [hive-addon.plug.source :as src]
            [hive-dsl.result :as r]
            [clojure.set :as set]))

;; SPDX-License-Identifier: MIT

(def deep-merge mrg/deep-merge)
(def merge-configs mrg/merge-configs)
(def lint lint/check)

(defn source-family
  "Coord family keyword (:local | :git | :mvn), or nil for an unknown coord."
  [coord]
  (some-> (src/coord->source coord) src/family))

(defn apply-profile
  "Partition plug libs by the named profile into {:kept #{lib} :excluded {lib reason}}."
  [config profile]
  (let [libs (set (keys (:iaddon/plugs config)))
        prof (get-in config [:iaddon/profiles profile])]
    (if (or (nil? profile) (nil? prof) (:all prof))
      {:kept libs :excluded {}}
      (let [only     (:only prof)
            except   (:except prof)
            kept     (cond->> libs
                       (seq only)   (filter only)
                       (seq except) (remove except)
                       true         set)
            excluded (into {} (for [l libs :when (not (kept l))]
                                [l (if (and (seq only) (not (only l))) :not-in-profile-only :profile-excepted)]))]
        {:kept kept :excluded excluded}))))

(defn drop-disabled
  "Drop plugs whose :enabled? is explicitly false → {:kept #{lib} :dropped {lib :disabled}}."
  [plugs libs]
  (reduce (fn [acc l]
            (if (false? (:enabled? (get plugs l)))
              (assoc-in acc [:dropped l] :disabled)
              (update acc :kept conj l)))
          {:kept #{} :dropped {}} libs))

(defn plug-capabilities
  "Every capability a plug claims — singular :capability ∪ plural :capabilities."
  [plug]
  (into (if-let [c (:capability plug)] #{c} #{}) (:capabilities plug)))

(defn- forbidden-capabilities [rules]
  (into #{} (for [[cap rl] rules :when (false? (:permit rl))] cap)))

(defn select-capabilities
  "Resolve capability contention among kept libs: drop providers of :permit-false
   capabilities, then per contended capability pick the :prefer winner or fail.
   (r/ok {:selected #{lib} :dropped {lib reason}}) | (r/err :iaddon/capability-conflict ...)."
  [config kept]
  (let [plugs     (:iaddon/plugs config)
        rules     (:iaddon/capabilities config)
        forbidden (forbidden-capabilities rules)
        denied    (into {} (for [l    kept
                                 :let [banned (set/intersection (plug-capabilities (get plugs l)) forbidden)]
                                 :when (seq banned)]
                             [l {:capability-permit-denied (vec banned)}]))
        live      (remove denied kept)
        by-cap    (reduce (fn [m l] (reduce (fn [m c] (update m c (fnil conj #{}) l)) m (plug-capabilities (get plugs l))))
                          {} live)
        resolved  (reduce
                   (fn [acc [cap libs]]
                     (let [prefer (get-in rules [cap :prefer])]
                       (cond
                         (<= (count libs) 1) acc
                         (contains? libs prefer)
                         (reduce (fn [a l] (if (= l prefer) a (assoc-in a [:dropped l] {:capability-lost-to prefer :capability cap}))) acc libs)
                         prefer (update acc :conflicts conj {:capability cap :providers (vec libs) :prefer-unavailable prefer})
                         :else  (update acc :conflicts conj {:capability cap :providers (vec libs)}))))
                   {:dropped {} :conflicts []} by-cap)]
    (if (seq (:conflicts resolved))
      (r/err :iaddon/capability-conflict {:conflicts (:conflicts resolved)})
      (r/ok {:selected (set/difference (set live) (set (keys (:dropped resolved))))
             :dropped  (merge denied (:dropped resolved))}))))

(defn plug-provenance
  "Per-plug field → the layer id that last set it (::layer = last layer touching the plug)."
  [layers]
  (reduce (fn [acc {:keys [id config]}]
            (reduce (fn [a [lib plug]]
                      (reduce (fn [a2 [f _]] (assoc-in a2 [lib f] id))
                              (assoc-in a [lib ::layer] id) plug))
                    acc (:iaddon/plugs config)))
          {} layers))

(defn resolve-config
  "Resolve layers → (r/ok {:config :selected :dropped :provenance}) or the first
   (r/err ...) from schema / lint / capability selection."
  [layers {:keys [profile]}]
  (r/let-ok [merged (schema/validate* schema/IaddonConfig (merge-configs (map :config layers)) :iaddon/schema-violation)
             _      (lint merged)
             prof   (r/ok (apply-profile merged profile))
             dis    (r/ok (drop-disabled (:iaddon/plugs merged) (:kept prof)))
             cap    (select-capabilities merged (:kept dis))]
    (r/ok {:config     merged
           :selected   (into {} (map (fn [l] [l (get-in merged [:iaddon/plugs l])])) (:selected cap))
           :dropped    (merge (:excluded prof) (:dropped dis) (:dropped cap))
           :provenance (plug-provenance layers)})))

(defn explain
  "Resolution as a flat report — :selected libs, :dropped reasons, :provenance — or the error."
  [layers opts]
  (let [res (resolve-config layers opts)]
    (if (r/ok? res)
      (let [{:keys [selected dropped provenance]} (:ok res)]
        {:status :ok :selected (vec (keys selected)) :dropped dropped :provenance provenance})
      {:status :error :error (:error res) :detail (dissoc res :error)})))
