(ns hive-addon.opaque.addon
  "The generic FOSS proxy: ONE IAddon record standing in for ANY opaque kernel.

   This is the Single-Source Lever of the marketplace story. A proprietary addon
   ships as a compiled binary and nothing else; the record that mounts it is
   this one, MIT-licensed, generic, and identical for every vendor. That the
   proxy is open is not a leak: it holds no kernel logic at all, only framing
   and delegation. All of the IP is in the binary.

   The kernel SELF-DESCRIBES, so this record learns the addon's tools, hooks,
   exclusions and schema extensions from the running kernel rather than from the
   manifest. A manifest that enumerated them would be a second copy of the truth
   and would rot the first time the vendor shipped a new build.

   What the manifest still supplies is what must be known BEFORE the kernel
   runs: the addon id and its advertised capabilities. The mount pipeline reads
   both while deciding whether this addon is licensed and where it sits in the
   dependency order, which is strictly before anything has been started."
  (:require [hive-addon.opaque.codec :as codec]
            [hive-addon.opaque.transport :as t]
            [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; One call across the wire
;; =============================================================================

(defn call!
  "Send one op to the kernel and answer its decoded RESPONSE MAP.

   Never throws. A dead pipe, an unreadable answer and a kernel-side error all
   come back as an :error response, because every caller here is an IAddon
   method and the IAddon contract has no exception channel: `health` must
   answer, `tools` must return a seq, a tool handler must return a result map."
  [transport op args]
  (let [answer (try
                 (some->> (codec/request op args)
                          (codec/encode)
                          (t/-request! transport)
                          (codec/decode))
                 (catch #?(:clj Throwable :default :default) t
                   (codec/error op (or (ex-message t) (str t)))))]
    (cond
      (nil? answer)  (codec/error op "kernel closed the pipe")
      (map? answer)  answer
      :else          (codec/error op (str "malformed response: " (pr-str answer))))))

(defn call-result!
  "The :result of one op, or `fallback` when the call failed. For the IAddon
   methods that must answer with a value of a fixed shape whatever went wrong."
  [transport op args fallback]
  (let [answer (call! transport op args)]
    (if (codec/error? answer)
      fallback
      (codec/result answer))))

;; =============================================================================
;; The self-description cache
;; =============================================================================

(defn- fetch-describe!
  "Ask the kernel to describe itself. nil when it cannot answer."
  [transport]
  (let [answer (call! transport :addon/describe nil)]
    (when-not (codec/error? answer)
      (codec/result answer))))

(defn- described
  "The kernel's self-description, fetching it once and caching it.

   Cached because `tools` is called on every tools/list and a round trip per
   call would be paid for nothing: a running kernel's surface does not change,
   and one that is restarted is a new mount. Returns nil while the kernel is not
   running, which every reader below treats as an empty surface rather than an
   error, so a host can enumerate a down addon's tools without crashing."
  [this]
  (or @(:describe-cache this)
      (when (t/-alive? (:transport this))
        (let [d (fetch-describe! (:transport this))]
          (when (map? d)
            (reset! (:describe-cache this) d)
            d)))))

;; =============================================================================
;; The proxy
;; =============================================================================

(defrecord OpaqueAddon [spec transport describe-cache]
  proto/IAddon

  (addon-id [this]
    ;; From the SPEC, never from the kernel: the mount pipeline keys the
    ;; registry by this before anything is started, and the licence gate checks
    ;; the entitlement against it before the constructor namespace is loaded.
    (:opaque/id (:spec this)))

  (addon-type [_this] :external)

  (capabilities [this]
    (or (:addon/capabilities (described this))
        (:opaque/capabilities (:spec this))
        #{}))

  (initialize! [this config]
    (let [start (try (t/-start! (:transport this)) nil
                     (catch #?(:clj Throwable :default :default) t
                       (or (ex-message t) (str t))))]
      (if (some? start)
        (codec/init-result false [(str "could not start the kernel: " start)])
        (let [answer (call! (:transport this) :addon/initialize! (or config {}))]
          (if (codec/error? answer)
            (codec/init-result false [(:error answer)])
            (let [d  (described this)
                  id (:opaque/id (:spec this))]
              (cond
                (nil? d)
                (codec/init-result false ["the kernel started but would not describe itself"])

                ;; The manifest promised an id and the binary disagrees. Refuse:
                ;; the licence gate has already decided in favour of the id the
                ;; MANIFEST named, so mounting the other one would run an addon
                ;; nobody checked an entitlement for.
                (and (some? id) (not= id (:addon/id d)))
                (codec/init-result
                 false
                 [(str "kernel identifies as " (pr-str (:addon/id d))
                       " but the manifest mounted it as " (pr-str id))])

                :else
                (merge (codec/init-result true)
                       {:metadata {:opaque/id         (:addon/id d)
                                   :opaque/tool-count (count (:addon/tools d))}}))))))))

  (shutdown! [this]
    ;; Ask first, then stop. A kernel holding a licence lease or a file handle
    ;; needs the chance to release it; -stop! alone is a kill.
    (call! (:transport this) :addon/shutdown! nil)
    (try (t/-stop! (:transport this)) (catch #?(:clj Throwable :default :default) _ nil))
    (reset! (:describe-cache this) nil)
    nil)

  (tools [this]
    (let [transport (:transport this)]
      (mapv (fn [summary]
              (assoc summary
                     :handler
                     (fn [params]
                       (let [answer (call! transport :addon/tool
                                           {:tool (:name summary) :params params})]
                         (if (codec/error? answer)
                           {:error (:error answer)}
                           (codec/result answer))))))
            (:addon/tools (described this)))))

  (schema-extensions [this]
    (or (:addon/schema-extensions (described this)) []))

  (health [this]
    (let [alive? (try (t/-alive? (:transport this))
                      (catch #?(:clj Throwable :default :default) _ false))]
      (if-not alive?
        (codec/health-report false {:opaque/id (:opaque/id (:spec this))})
        ;; Ask the kernel rather than reporting on the pipe. A live process whose
        ;; own health is :degraded must not be reported :ok just because its
        ;; stdin is open, and a process that has stopped answering is :down
        ;; however alive the OS thinks it is.
        (let [answer (call! (:transport this) :addon/health nil)]
          (if (codec/error? answer)
            (codec/health-report false {:opaque/id  (:opaque/id (:spec this))
                                        :last-error (:error answer)})
            (let [report (codec/result answer)]
              (if (map? report)
                report
                (codec/health-report true {:opaque/id (:opaque/id (:spec this))}))))))))

  (excluded-tools [this]
    (set (:addon/excluded-tools (described this))))

  (hooks [this]
    (let [transport (:transport this)]
      (reduce-kv
       (fn [acc k summary]
         (assoc acc k
                (if (= :data (:kind summary))
                  (:value summary)
                  ;; A fn-valued hook becomes a callback. The host calls it with
                  ;; whatever the ext-key surface passes; the args cross as data.
                  (fn [& args]
                    (call-result! transport :addon/hook
                                  {:key k :args (vec args)} nil)))))
       {}
       (:addon/hooks (described this))))))

(defn opaque-addon
  "Construct the proxy from an OpaqueSpec and an ITransport. The spec is data
   and the transport is behaviour, which is why one is injected and the other
   is described (Cardinality Decides the Construct)."
  [spec transport]
  (->OpaqueAddon spec transport (atom nil)))
