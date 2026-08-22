(ns hive-addon.hot-fixture
  "Probe addons for the hot-reload suite.

   Constructors live in a real namespace with real vars because that is what the
   bridge exercises: `resolve-constructor` reaches them through
   `requiring-resolve` on the manifest's :addon/init-ns + :addon/init-fn, so a
   fixture built from inline `reify` would test nothing.

   Every construction bumps a per-id generation counter, so a test can assert
   that a remount produced a NEW instance rather than re-initializing the old
   one — the distinction the whole bridge turns on. The injected
   :mount/dependencies are captured too, so sibling injection can be checked
   across a remount."
  (:require [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defonce generations (atom {}))
(defonce events (atom []))

(defn reset-fixture!
  "Clear generation counters and the event log."
  []
  (reset! generations {})
  (reset! events [])
  nil)

(defn log! [event] (swap! events conj event) nil)

(defn events-of
  "Event log entries of a given kind, in order."
  [kind]
  (filterv #(= kind (first %)) @events))

;; =============================================================================
;; The probe addon
;; =============================================================================

(defrecord ProbeAddon [id generation config]
  proto/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] (:probe/capabilities config #{:tools}))
  (initialize! [_ cfg]
    (log! [:init id generation (set (keys (:mount/dependencies cfg {})))])
    {:success? true :errors []})
  (shutdown! [_]
    (log! [:shutdown id generation])
    nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok :details {:generation generation}})
  (excluded-tools [_] #{})
  (hooks [_] {}))

(defn- construct
  "Build a probe addon for `id`, bumping its generation counter."
  [id config]
  (let [gen (get (swap! generations update id (fnil inc 0)) id)]
    (log! [:construct id gen])
    (->ProbeAddon id gen config)))

;; =============================================================================
;; Constructors named by the fixture manifests
;; =============================================================================

(defn make-a [config] (construct "probe.a" config))
(defn make-b [config] (construct "probe.b" config))
(defn make-c [config] (construct "probe.c" config))

(defn make-shared
  "Two distinct addons construct from THIS one var — the shared-namespace case
   that `seeds-for-ns` must seed twice (as hive.qdrant and hive.qdrant.kanban
   really do in the fleet)."
  [config]
  (construct (:probe/id config "probe.shared") config))

(defn make-broken
  "A constructor that throws, to exercise graceful degrade on remount."
  [_config]
  (throw (ex-info "probe constructor deliberately failed" {})))

(defn make-nil
  "A constructor returning nil — the other graceful-degrade shape."
  [_config]
  nil)
