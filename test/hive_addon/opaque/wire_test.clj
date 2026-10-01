(ns hive-addon.opaque.wire-test
  "End-to-end coverage for the opaque seam: a live IAddon served by
   hive-addon.opaque.serve and mounted through hive-addon.opaque.addon.

   The transport is the in-process one, and that is not a weakening. It moves
   the SAME lines the subprocess moves, framed by the same codec, so everything
   proven here holds for a compiled binary too; what the subprocess adds is a
   pipe and a PID, which is a property of the transport rather than of the
   proxy. The one thing an in-process transport cannot prove is that a `cljw
   build` artifact behaves this way, and no unit test can prove that: it needs
   the binary, and that evidence belongs at a different rung.

   What is actually at stake here is a marketplace promise: an addon that works
   in-process must work identically once it is compiled opaque. Every deftest
   below is one way that promise could quietly break."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-addon.mount.schema :as ms]
            [hive-addon.opaque :as opaque]
            [hive-addon.opaque.addon :as oaddon]
            [hive-addon.opaque.codec :as codec]
            [hive-addon.opaque.serve :as serve]
            [hive-addon.opaque.transport :as t]
            [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; A stand-in for a vendor's proprietary kernel
;; =============================================================================

(defrecord AcmeKernel [state]
  proto/IAddon
  (addon-id [_] "acme.rank")
  (addon-type [_] :external)
  (capabilities [_] #{:tools :vector-search})
  (initialize! [this config]
    (reset! (:state this) {:up? true :config config})
    {:success? true :errors [] :metadata {:seeded (count config)}})
  (shutdown! [this] (swap! (:state this) assoc :up? false) nil)
  (tools [_]
    [{:name "rank"
      :description "the vendor's secret ranking"
      :inputSchema {:type "object" :properties {"x" {:type "number"}}}
      :handler (fn [{:keys [x]}] {:result (* 3 x)})}])
  (schema-extensions [_] [{:addon.acme/score {:db/valueType :db.type/long}}])
  (health [this] {:status (if (:up? @(:state this)) :ok :down) :details {:vendor "acme"}})
  (excluded-tools [_] #{"rank_basic"})
  (hooks [_] {:acme/score (fn [n] {:scored n})
              :acme/table {:a 1 :b 2}}))

(defn- kernel [] (->AcmeKernel (atom {})))

(def ^:private spec
  {:opaque/exec "/opt/acme/acme-rank"
   :opaque/id "acme.rank"
   :opaque/capabilities #{:tools}})

(defn- mounted
  "A started proxy over a live kernel, plus the kernel, for assertions on both
   ends of the wire."
  ([] (mounted (kernel)))
  ([k]
   (let [proxy (opaque/inproc-addon spec k)]
     (proto/initialize! proxy {:addon/id "acme.rank"})
     {:proxy proxy :kernel k})))

;; =============================================================================
;; The whole IAddon surface survives the crossing
;; =============================================================================

(deftest every-iaddon-method-round-trips
  (let [{:keys [proxy]} (mounted)]
    (testing "identity and type come from the manifest, not from the kernel"
      ;; They must answer BEFORE anything is running: the mount pipeline keys
      ;; the registry by the id and the licence gate checks the entitlement
      ;; against it, both strictly before the constructor namespace is loaded.
      (is (= "acme.rank" (proto/addon-id proxy)))
      (is (= :external (proto/addon-type proxy))))

    (testing "capabilities come from the running kernel, which knows more"
      (is (= #{:tools :vector-search} (proto/capabilities proxy))))

    (testing "tools arrive with a working handler the proxy supplied"
      (let [tools (proto/tools proxy)]
        (is (= ["rank"] (mapv :name tools)))
        (is (= "the vendor's secret ranking" (:description (first tools))))
        (is (= {:result 42} ((:handler (first tools)) {:x 14})))
        (testing "and the handler normalizes MCP's string keys"
          (is (= {:result 42} ((:handler (first tools)) {"x" 14}))))))

    (testing "health is the KERNEL's answer, not a liveness check on the pipe"
      ;; A live process whose own health is :degraded must not read :ok merely
      ;; because its stdin is open.
      (is (= {:status :ok :details {:vendor "acme"}} (proto/health proxy))))

    (testing "the remaining pure surface crosses as data"
      (is (= #{"rank_basic"} (proto/excluded-tools proxy)))
      (is (= [{:addon.acme/score {:db/valueType :db.type/long}}]
             (proto/schema-extensions proxy))))

    (testing "a data hook travels; a fn hook becomes a callback"
      (let [hooks (proto/hooks proxy)]
        (is (= {:a 1 :b 2} (get hooks :acme/table)))
        (is (= {:scored 7} ((get hooks :acme/score) 7)))))))

(deftest shutdown-asks-before-it-stops
  (let [{:keys [proxy kernel]} (mounted)]
    (is (nil? (proto/shutdown! proxy)))
    (testing "the kernel got its chance to release what it holds"
      ;; -stop! alone is a kill. A kernel holding a licence lease or a file
      ;; handle needs the request first, and only the kernel's own state can
      ;; witness that it arrived.
      (is (false? (:up? @(:state kernel)))))
    (testing "and afterwards the addon reports itself down"
      (is (= :down (:status (proto/health proxy)))))))

(deftest a-down-addon-is-enumerable-not-explosive
  ;; A host lists tools across every mounted addon. One dead kernel must not
  ;; take tools/list down with it, so every reader treats an unreachable kernel
  ;; as an empty surface.
  (let [proxy (opaque/inproc-addon spec (kernel))]
    (testing "before initialize!, with nothing started"
      (is (= [] (proto/tools proxy)))
      (is (= #{} (proto/excluded-tools proxy)))
      (is (= {} (proto/hooks proxy)))
      (is (= [] (proto/schema-extensions proxy)))
      (is (= :down (:status (proto/health proxy))))
      (testing "and capabilities fall back to what the manifest advertised"
        (is (= #{:tools} (proto/capabilities proxy)))))))

;; =============================================================================
;; The kernel is allowed to be a legacy addon
;; =============================================================================

(deftest a-kernel-may-omit-the-optional-methods
  ;; excluded-tools and hooks are optional per the protocol and the host
  ;; registry defaults them. The kernel must default them the SAME way, or an
  ;; addon that mounts in-process today would stop mounting the moment it is
  ;; compiled opaque, which is the one regression a marketplace cannot ship.
  (let [legacy (reify proto/IAddon
                 (addon-id [_] "acme.legacy")
                 (addon-type [_] :external)
                 (capabilities [_] #{:tools})
                 (initialize! [_ _] {:success? true :errors []})
                 (shutdown! [_] nil)
                 (tools [_] [])
                 (schema-extensions [_] [])
                 (health [_] {:status :ok}))
        described (serve/describe legacy)]
    (is (= #{} (:addon/excluded-tools described)))
    (is (= {} (:addon/hooks described)))
    (let [proxy (opaque/inproc-addon {:opaque/id "acme.legacy"} legacy)]
      (is (:success? (proto/initialize! proxy {})))
      (is (= #{} (proto/excluded-tools proxy)))
      (is (= {} (proto/hooks proxy))))))

(deftest a-method-that-genuinely-throws-is-not-mistaken-for-an-absent-one
  ;; The optional-method skip recognizes non-implementation by MESSAGE, so a
  ;; method that throws for its own reasons must still surface as a failure.
  (let [broken (reify proto/IAddon
                 (addon-id [_] "acme.broken")
                 (addon-type [_] :external)
                 (capabilities [_] #{})
                 (initialize! [_ _] {:success? true :errors []})
                 (shutdown! [_] nil)
                 (tools [_] (throw (ex-info "vendor bug" {})))
                 (schema-extensions [_] [])
                 (health [_] {:status :ok})
                 (excluded-tools [_] #{})
                 (hooks [_] {}))
        answer (serve/respond broken (codec/encode (codec/request :addon/describe)))]
    (is (codec/error? answer))
    (is (= "vendor bug" (:error answer)))
    (testing "and the op is echoed, so the host knows WHICH call failed"
      (is (= :addon/describe (:op answer))))))

;; =============================================================================
;; Failures that must be answers, not crashes
;; =============================================================================

(deftest the-kernel-answers-garbage-instead-of-dying
  ;; A kernel that dies on a malformed line takes the addon down with it, and
  ;; the host cannot tell that from a vendor bug. Every one of these is a
  ;; response line.
  (let [k (kernel)]
    (is (= {:error "unreadable request line"} (serve/respond k "{:op :addon/health")))
    (is (= {:error "request line is not a map"} (serve/respond k "42")))
    (is (= {:error "unknown op: :addon/teleport"}
           (serve/respond k (codec/encode {:op :addon/teleport}))))
    (testing "an unknown tool names itself, since that is a manifest bug"
      (is (= {:op :addon/tool :error "no such tool: \"nope\""}
             (serve/respond k (codec/encode (codec/request :addon/tool {:tool "nope"}))))))
    (testing "and an unknown hook likewise"
      (is (= {:op :addon/hook :error "no such hook: :nope/x"}
             (serve/respond k (codec/encode (codec/request :addon/hook {:key :nope/x}))))))))

(deftest the-proxy-answers-a-dead-pipe-instead-of-throwing
  ;; Every caller of `call!` is an IAddon method, and IAddon has no exception
  ;; channel: health must answer, tools must return a seq, a handler must return
  ;; a result map. A transport that has stopped answering exercises all three.
  (let [dead  (opaque/line-transport (fn [_line] nil))
        proxy (oaddon/opaque-addon spec dead)]
    (t/-start! dead)
    (is (codec/error? (oaddon/call! dead :addon/health nil)))
    (is (= "kernel closed the pipe" (:error (oaddon/call! dead :addon/health nil))))
    (is (= [] (proto/tools proxy)))
    (is (= :down (:status (proto/health proxy)))))

  (testing "and a transport that throws is an error response, not a throw"
    (let [angry (opaque/line-transport (fn [_line] (throw (ex-info "pipe reset" {}))))
          proxy (oaddon/opaque-addon spec angry)]
      (t/-start! angry)
      (is (= "pipe reset" (:error (oaddon/call! angry :addon/health nil))))
      (is (= :down (:status (proto/health proxy)))))))

(deftest a-kernel-that-will-not-start-fails-the-mount-cleanly
  (let [refuses (reify t/ITransport
                  (-start! [_] (throw (ex-info "binary not found" {})))
                  (-request! [_ _] nil)
                  (-alive? [_] false)
                  (-stop! [_] nil))
        proxy   (oaddon/opaque-addon spec refuses)
        result  (proto/initialize! proxy {})]
    (is (false? (:success? result)))
    (is (= ["could not start the kernel: binary not found"] (:errors result)))))

(deftest a-kernel-whose-identity-disagrees-with-the-manifest-is-refused
  ;; The licence gate has already decided in favour of the id the MANIFEST
  ;; named. Mounting a binary that calls itself something else would run an
  ;; addon nobody checked an entitlement for, so the mismatch fails the mount
  ;; rather than being reconciled in favour of either side.
  (let [proxy  (opaque/inproc-addon {:opaque/id "acme.licensed"} (kernel))
        result (proto/initialize! proxy {})]
    (is (false? (:success? result)))
    (is (= ["kernel identifies as \"acme.rank\" but the manifest mounted it as \"acme.licensed\""]
           (:errors result)))))

;; =============================================================================
;; The bytes themselves
;; =============================================================================

(deftest the-proxy-sends-exactly-the-lines-the-kernel-reads
  ;; The one assertion a faithful in-process transport cannot make for you: what
  ;; is ON the wire. A recording line function is the only place the actual
  ;; bytes are visible, and they are what a vendor's compiled kernel will parse.
  (let [sent      (atom [])
        recording (opaque/line-transport
                   (fn [line]
                     (swap! sent conj line)
                     (serve/handle-line (kernel) line)))
        proxy     (oaddon/opaque-addon spec recording)]
    (proto/initialize! proxy {:tuning 0.5})
    ((:handler (first (proto/tools proxy))) {:x 2})
    (let [lines (mapv codec/decode @sent)]
      (is (= [:addon/initialize! :addon/describe :addon/tool] (mapv :op lines)))
      (is (= {:tuning 0.5} (:args (first lines))))
      (is (= {:tool "rank" :params {:x 2}} (:args (last lines))))
      (testing "one line per request, with no embedded newline to desynchronize
                a line-oriented reader"
        (is (every? (fn [l] (not (clojure.string/includes? l "\n"))) @sent))))))

(deftest describe-is-fetched-once-and-cached
  ;; tools is called on every tools/list. A round trip per call would be paid
  ;; for nothing: a running kernel's surface does not change, and one that is
  ;; restarted is a new mount.
  (let [calls     (atom 0)
        counting  (opaque/line-transport
                   (fn [line]
                     (when (= :addon/describe (:op (codec/decode line)))
                       (swap! calls inc))
                     (serve/handle-line (kernel) line)))
        proxy     (oaddon/opaque-addon spec counting)]
    (proto/initialize! proxy {})
    (dotimes [_ 5] (proto/tools proxy))
    (proto/excluded-tools proxy)
    (proto/hooks proxy)
    (is (= 1 @calls))))

;; =============================================================================
;; The mount manifest
;; =============================================================================

(deftest an-opaque-addon-mounts-through-an-ORDINARY-manifest
  ;; The whole design rests on this: no protocol change, no mounter change, no
  ;; manifest shape of its own. If the opaque seam ever needed one, this
  ;; assertion is what would stop first.
  (let [manifest (opaque/->manifest spec)]
    (is (ms/validate ms/MountSpec manifest)
        (str "manifest is not a MountSpec: " (ms/humanize-errors ms/MountSpec manifest)))
    (is (= :external (:addon/type manifest)))
    (testing "trust-class is what puts it under the licence gate"
      (is (= :proprietary (:addon/trust-class manifest)))
      (is (= "acme.rank" (:addon/entitlement manifest))))
    (testing "the constructor is the generic one, the same for every vendor"
      (is (= "hive-addon.opaque" (:addon/init-ns manifest)))
      (is (= "addon-ctor" (:addon/init-fn manifest))))
    (testing "capabilities cover what the host must know before starting it"
      (is (= #{:tools :health-reporting} (:addon/capabilities manifest))))
    (testing "and the kernel's TOOLS are absent on purpose"
      ;; They would be a second copy of the truth, and would rot the first time
      ;; the vendor shipped a build with a new tool.
      (is (not (contains? manifest :addon/tools))))))

(deftest addon-ctor-reconstructs-the-proxy-from-the-manifest
  (let [manifest (opaque/->manifest spec)
        built    (opaque/addon-ctor manifest)]
    (is (proto/addon? built))
    (is (= "acme.rank" (proto/addon-id built)))
    (is (= :external (proto/addon-type built)))
    (testing "and a config with no spec builds nothing, rather than a broken proxy"
      (is (nil? (opaque/addon-ctor {}))))))

(deftest emit-writes-the-manifest-where-a-host-scans
  (let [dir  (str (System/getProperty "java.io.tmpdir")
                  "/hive-addon-opaque-test-" (System/currentTimeMillis))
        path (opaque/emit! spec dir)]
    (try
      (is (clojure.string/ends-with? path "META-INF/hive-addons/acme.rank.edn"))
      (let [written (read-string (slurp path))]
        (is (= (opaque/->manifest spec) written))
        (testing "keys stay flat, so the file is readable by anything that
                  reads EDN rather than only by a Clojure reader"
          (is (clojure.string/includes? (slurp path) ":addon/id"))
          (is (not (clojure.string/includes? (slurp path) "#:addon")))))
      (finally
        (doseq [f (reverse (file-seq (io/file dir)))] (io/delete-file f true))))))

;; =============================================================================
;; The vendor's entry point
;; =============================================================================

(deftest entry-source-does-its-work-at-top-level
  ;; Measured on cljw: a `build` artifact receives no *command-line-args* and
  ;; its main is never invoked, while a top-level read-line loop works. An entry
  ;; that defines a tidy -main compiles cleanly and then does nothing, which is
  ;; the worst possible failure for a shipped binary.
  (let [src (opaque/entry-source '[[acme.kernel :as k]] 'acme.kernel/make)]
    (is (clojure.string/includes? src "(serve/serve! (acme.kernel/make))"))
    (is (clojure.string/includes? src "[acme.kernel :as k]"))
    (is (clojure.string/includes? src "hive-addon.opaque.serve"))
    (is (not (clojure.string/includes? src "defn -main"))
        "an uncalled -main is the failure this entry shape exists to avoid")
    (testing "and the entry namespace is not NAMED after one either, so the
              check above cannot pass or fail on the ns name"
      (is (clojure.string/includes? src "(ns kernel-entry"))))
  (testing "and it reads back as forms, so a build cannot fail on a typo here"
    (let [src (opaque/entry-source [] 'acme.kernel/make)]
      (is (= 2 (count (read-string (str "[" src "]")))))))

  (testing "the entry ns is a PARAMETER, because two build paths name it
            differently and neither may be hardcoded"
    ;; A vendor running `cljw build` takes the default. hive-native's opacity
    ;; pipeline stages the entry as kernel-main and builds that symbol, so it
    ;; passes its own name. Hardcoding either one makes this emitter unusable
    ;; from the other path, which is how a second copy of the entry gets
    ;; written and how the two copies then drift.
    (let [src (opaque/entry-source '[[acme.kernel :as k]] 'acme.kernel/make 'kernel-main)]
      (is (clojure.string/includes? src "(ns kernel-main"))
      (is (clojure.string/includes? src "(serve/serve! (acme.kernel/make))")))))
