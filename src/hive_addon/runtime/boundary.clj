(ns hive-addon.runtime.boundary
  "Effectful boundary for client runtimes: runs the plans built by
   hive-addon.runtime.plan and provisions an injected addon's runtimes.

   Effects go through two injected ports: IRuntimeFs for files and an eval-fn
   (fn [client commands]) for clients that are already running. `local-fs` is
   the production filesystem; `recording-fs` records effects for tests and can
   fail on demand. `run-plan!` never throws: it stops at the first failing
   effect and reports the rest as skipped.

   `provisioner` is the composition a host passes to hive-addon.mount/mount!
   as :provision, and to teardown! as :deprovision.

   Rationale lives in hive memory (KG-linked), not here."
  (:require [hive-addon.protocol :as proto]
            [hive-addon.runtime.plan :as plan]
            [hive-addon.runtime.schema :as rs]
            [malli.core :as m])
  (:import [java.nio.file CopyOption Files LinkOption Path Paths StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Filesystem port
;; =============================================================================

(defprotocol IRuntimeFs
  (delete-tree! [fs path] "Remove PATH and everything under it; absent is fine.")
  (copy-tree! [fs from to] "Copy the directory FROM to TO, creating TO.")
  (write-file! [fs path content] "Write CONTENT to PATH atomically, creating parents."))

(defn- ->path
  ^Path [s]
  (Paths/get (str s) (make-array String 0)))

(defn- exists?
  [^Path p]
  (Files/exists p (make-array LinkOption 0)))

(defrecord LocalFs []
  IRuntimeFs
  (delete-tree! [_ path]
    (let [root (->path path)]
      (when (exists? root)
        (with-open [walk (Files/walk root (make-array java.nio.file.FileVisitOption 0))]
          (doseq [^Path p (reverse (vec (iterator-seq (.iterator walk))))]
            (Files/delete p))))
      nil))
  (copy-tree! [_ from to]
    (let [src (->path from)
          dst (->path to)]
      (when-not (Files/isDirectory src (make-array LinkOption 0))
        (throw (ex-info (str "runtime source is not a directory: " from) {:from from})))
      (with-open [walk (Files/walk src (make-array java.nio.file.FileVisitOption 0))]
        (doseq [^Path p (vec (iterator-seq (.iterator walk)))]
          (let [target (.resolve dst (.relativize src p))]
            (if (Files/isDirectory p (make-array LinkOption 0))
              (Files/createDirectories target (make-array FileAttribute 0))
              (Files/copy p target
                          ^"[Ljava.nio.file.CopyOption;"
                          (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))))))
      nil))
  (write-file! [_ path content]
    (let [target (->path path)
          parent (.getParent target)
          _ (Files/createDirectories parent (make-array FileAttribute 0))
          tmp (Files/createTempFile parent ".hive-runtime" ".tmp" (make-array FileAttribute 0))]
      (spit (.toFile tmp) content)
      (Files/move tmp target
                  ^"[Ljava.nio.file.CopyOption;"
                  (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING
                                          StandardCopyOption/ATOMIC_MOVE]))
      nil)))

(defn local-fs
  "The production filesystem."
  []
  (->LocalFs))

(defrecord RecordingFs [log fail-on]
  IRuntimeFs
  (delete-tree! [_ path]
    (when (contains? fail-on :fs/delete-tree) (throw (ex-info "delete failed" {:path path})))
    (swap! log conj {:effect :fs/delete-tree :path path})
    nil)
  (copy-tree! [_ from to]
    (when (contains? fail-on :fs/copy-tree) (throw (ex-info "copy failed" {:from from})))
    (swap! log conj {:effect :fs/copy-tree :from from :to to})
    nil)
  (write-file! [_ path content]
    (when (contains? fail-on :fs/write) (throw (ex-info "write failed" {:path path})))
    (swap! log conj {:effect :fs/write :path path :content content})
    nil))

(defn recording-fs
  "A filesystem that records every effect in (:log fs) and throws on the effect
   kinds in FAIL-ON."
  ([] (recording-fs #{}))
  ([fail-on] (->RecordingFs (atom []) (set fail-on))))

;; =============================================================================
;; Plan interpreter (open by effect kind)
;; =============================================================================

(defmulti run-effect!
  "Perform one Effect with the ports in CTX ({:fs IRuntimeFs :eval-fn f})."
  (fn [_ctx effect] (:effect effect)))

(defmethod run-effect! :fs/delete-tree [{:keys [fs]} {:keys [path]}]
  (delete-tree! fs path))

(defmethod run-effect! :fs/copy-tree [{:keys [fs]} {:keys [from to]}]
  (copy-tree! fs from to))

(defmethod run-effect! :fs/write [{:keys [fs]} {:keys [path content]}]
  (write-file! fs path content))

(defmethod run-effect! :client/eval [{:keys [eval-fn]} {:keys [client commands]}]
  (when-not eval-fn
    (throw (ex-info "no client eval-fn was injected" {:client client})))
  (eval-fn client commands))

(defn run-plan!
  "Run PLAN's effects in order. Returns a vector of Outcome, one per effect;
   after the first failure the remaining effects are not run and are reported
   :skipped?. Never throws."
  [ctx plan]
  (loop [[effect & more] plan
         failed? false
         acc []]
    (if-not effect
      acc
      (if failed?
        (recur more true (conj acc {:effect (:effect effect) :ok? false :skipped? true}))
        (let [outcome (try
                        (run-effect! ctx effect)
                        {:effect (:effect effect) :ok? true}
                        (catch Throwable t
                          {:effect (:effect effect) :ok? false
                           :error (or (ex-message t) (str t))}))]
          (recur more (not (:ok? outcome)) (conj acc outcome)))))))

(defn plan-ok?
  [outcomes]
  (every? :ok? outcomes))

;; =============================================================================
;; Provisioning an injected addon
;; =============================================================================

(defn declarations
  "The runtimes to provision for SPEC: its own :addon/runtime followed by any a
   host supplies in EXTRA, a map of addon id to RuntimeDecls."
  [spec extra]
  (vec (concat (:addon/runtime spec) (get extra (:addon/id spec)))))

(defn- hook-value
  [instance k]
  (let [v (get (proto/hooks instance) k)]
    (if (fn? v) (v) v)))

(defn- resolve-ref
  "A Source or Binding value: its literal, or the named hook of INSTANCE."
  [instance literal hook]
  (if hook
    (let [v (hook-value instance hook)]
      (when (nil? v)
        (throw (ex-info (str "addon hook " hook " is absent or returned nil") {:hook hook})))
      (str v))
    literal))

(defn- failed-runtime
  [decl message]
  {:runtime/id (str (:runtime/id decl))
   :runtime/client (if (keyword? (:runtime/client decl)) (:runtime/client decl) :unknown)
   :ok? false
   :outcomes []
   :error message})

(defn provision-runtime!
  "Install DECL for INSTANCE and, when :live? and an :eval-fn are given,
   activate it in running clients. Returns a RuntimeReport. Never throws.

   opts: {:fs :eval-fn :profiles :home :live?}. Live activation failing does
   not fail the install: it is reported under :live-ok?."
  [{:keys [fs eval-fn profiles home live?] :as opts} instance decl]
  (try
    (if-not (rs/valid-decl? decl)
      (failed-runtime decl (str "invalid runtime declaration: "
                                (pr-str (:errors (rs/explain-decl decl)))))
      (let [profile (get (or profiles plan/default-profiles) (:runtime/client decl))]
        (if-not profile
          (failed-runtime decl (str "no client profile for " (:runtime/client decl)))
          (let [{:source/keys [dir hook]} (:runtime/source decl)
                source-dir (resolve-ref instance dir hook)
                values (into {}
                             (map (fn [[k {:binding/keys [value hook]}]]
                                    [k (resolve-ref instance value hook)]))
                             (:runtime/bindings decl))
                {:keys [error plan] install-dir :dir}
                (plan/install-plan profile decl {:home home :source-dir source-dir :values values})]
            (if error
              (failed-runtime decl error)
              (let [outcomes (run-plan! opts plan)
                    installed? (plan-ok? outcomes)
                    live (when (and installed? live? eval-fn)
                           (run-plan! opts (plan/live-plan profile install-dir)))]
                (cond-> {:runtime/id (:runtime/id decl)
                         :runtime/client (:runtime/client decl)
                         :ok? installed?
                         :dir install-dir
                         :outcomes (into outcomes live)}
                  live (assoc :live-ok? (plan-ok? live)))))))))
    (catch Throwable t
      (failed-runtime decl (or (ex-message t) (str t))))))

(defn provisioner
  "A host's provisioning seam for hive-addon.mount: {:provision (fn [spec
   instance]) :deprovision (fn [addon-id]) :installed (fn [])}.

   :provision returns a ProvisionReport, or nil when SPEC declares no runtime
   and the host supplies none. What was installed is remembered per addon id,
   so :deprovision removes exactly that.

   opts: {:fs IRuntimeFs (default local-fs), :eval-fn (fn [client commands]),
          :profiles (default plan/default-profiles), :home (required),
          :declarations {addon-id RuntimeDecls}, :live? (default true)}"
  [{:keys [declarations home] :as opts}]
  (when-not (string? home)
    (throw (ex-info ":home is required" {})))
  (let [opts (merge {:fs (local-fs) :live? true :profiles plan/default-profiles} opts)
        installed (atom {})]
    {:provision
     (fn [spec instance]
       (let [decls (hive-addon.runtime.boundary/declarations spec declarations)
             id (:addon/id spec)]
         (when (seq decls)
           (let [reports (mapv #(provision-runtime! opts instance %) decls)]
             (swap! installed assoc id
                    (vec (keep (fn [[decl report]] (when (:ok? report) decl))
                               (map vector decls reports))))
             {:addon/id id
              :ok? (every? :ok? reports)
              :runtimes reports}))))
     :deprovision
     (fn [addon-id]
       (when-let [decls (seq (get @installed addon-id))]
         (let [profiles (:profiles opts)
               reports (mapv (fn [decl]
                               (let [profile (get profiles (:runtime/client decl))
                                     outcomes (run-plan! opts (plan/uninstall-plan profile decl home))]
                                 {:runtime/id (:runtime/id decl)
                                  :runtime/client (:runtime/client decl)
                                  :ok? (plan-ok? outcomes)
                                  :outcomes outcomes}))
                             decls)]
           (swap! installed dissoc addon-id)
           {:addon/id addon-id
            :ok? (every? :ok? reports)
            :runtimes reports})))
     :installed (fn [] @installed)}))

(m/=> run-plan! [:=> [:cat :map rs/Plan] [:vector rs/Outcome]])
