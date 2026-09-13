(ns hive-addon.runtime-test
  "Client-runtime provisioning: the pure plan, the effect interpreter, the
   provisioner over a real temp filesystem, the mount wiring, and a real Vim
   loading what was installed (skipped without vim)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.mount :as mount]
            [hive-addon.protocol :as proto]
            [hive-addon.runtime.boundary :as rb]
            [hive-addon.runtime.plan :as rp]
            [hive-addon.runtime.schema :as rs]
            [malli.core :as m]
            [malli.generator :as mg])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- temp-dir
  []
  (.toFile (Files/createTempDirectory "hive-runtime-test" (make-array FileAttribute 0))))

(defn- delete-tree!
  [f]
  (rb/delete-tree! (rb/local-fs) (str f)))

(defn- source-dir!
  "A runtime source with a plugin file and a nested autoload file."
  [root]
  (let [src (io/file root "src")]
    (io/make-parents (io/file src "plugin" "demo.vim"))
    (spit (io/file src "plugin" "demo.vim") "let g:demo_plugin_loaded = 1\n")
    (io/make-parents (io/file src "autoload" "demo" "deep.vim"))
    (spit (io/file src "autoload" "demo" "deep.vim") "\" autoload\n")
    src))

(def ^:private test-hooks (atom {}))

(defrecord HookAddon [id init-ok?]
  proto/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] #{})
  (initialize! [_ _] (if init-ok? {:success? true} {:success? false :errors ["init refused"]}))
  (shutdown! [_] nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] @test-hooks))

(defn ok-ctor [_] (->HookAddon "runtime.demo" true))

(defn failing-ctor [_] (->HookAddon "runtime.failing" false))

(defn- decl
  ([] (decl {}))
  ([overrides]
   (merge {:runtime/id "demo"
           :runtime/client :vim
           :runtime/source {:source/hook :demo/dir}
           :runtime/bindings {:port-file {:binding/hook :demo/port-file}}
           :runtime/on-load ["let g:demo_port_file = '{{port-file}}'"]}
          overrides)))

;; =============================================================================
;; Pure plan
;; =============================================================================

(deftest install-dir-expands-home-and-appends-the-runtime-id
  (is (= "/home/u/.vim/pack/hive/start/demo"
         (rp/install-dir (:vim rp/default-profiles) "demo" "/home/u")))
  (is (= "/home/u/.local/share/nvim/site/pack/hive/start/demo"
         (rp/install-dir (:nvim rp/default-profiles) "demo" "/home/u/")))
  (is (every? #(m/validate rs/ClientProfile %) (vals rp/default-profiles))))

(deftest unbound-placeholders-refuse-the-plan
  (is (= [:port-file] (rp/unbound (decl) {})))
  (is (= [] (rp/unbound (decl) {:port-file "/p"})))
  (is (= {:error "runtime demo has unbound placeholders: port-file"}
         (rp/install-plan (:vim rp/default-profiles) (decl) {:home "/h" :source-dir "/s" :values {}}))))

(def ^:private gen-binding-name
  (gen/fmap #(keyword (str "k" %)) (gen/choose 0 99)))

(defspec substitution-leaves-no-placeholder-it-was-given-a-value-for 200
  (prop/for-all [names (gen/vector-distinct gen-binding-name {:min-elements 1 :max-elements 5})
                 values (gen/vector (gen/such-that #(not (str/includes? % "{{")) gen/string-alphanumeric) 5)]
    (let [bindings (zipmap names (cycle values))
          s (str/join " " (map #(str "{{" (name %) "}}") names))
          out (rp/substitute s bindings)]
      (and (empty? (rp/placeholders out))
           (= out (str/join " " (map #(get bindings %) names)))))))

(defspec every-install-plan-is-a-valid-plan-ending-in-the-loader 100
  (prop/for-all [id (gen/fmap #(str "r" %) gen/string-alphanumeric)
                 commands (gen/vector gen/string-alphanumeric 0 4)]
    (let [profile (:vim rp/default-profiles)
          d {:runtime/id (subs id 0 (min 60 (count id))) :runtime/client :vim
             :runtime/source {:source/dir "/src"} :runtime/on-load commands}
          {:keys [dir plan]} (rp/install-plan profile d {:home "/h" :source-dir "/src" :values {}})]
      (and (rs/valid-decl? d)
           (m/validate rs/Plan plan)
           (= [:fs/delete-tree :fs/copy-tree :fs/write] (mapv :effect plan))
           (= dir (:path (first plan)))
           (str/ends-with? (:path (last plan)) "plugin/zz_hive_runtime.vim")
           (every? #(str/includes? (:content (last plan)) %) commands)))))

(deftest the-vim-loader-runs-once-now-or-at-vim-enter
  (let [src (rp/loader-source (:vim rp/default-profiles) "hive-carto.flow" ["Hello" "let g:x = 1"])]
    (is (str/includes? src "if exists('g:loaded_hive_runtime_hive_carto_flow')"))
    (is (str/includes? src "  Hello\n  let g:x = 1\n"))
    (is (str/includes? src "if v:vim_did_enter"))
    (is (str/includes? src "autocmd VimEnter * ++once call s:activate()")))
  (testing "live activation extends runtimepath, then sources autoload before plugins"
    (let [[rtp stale source] (:commands (first (rp/live-plan (:vim rp/default-profiles) "/r/it's")))]
      (is (str/includes? rtp "fnameescape('/r/it''s')"))
      (is (str/starts-with? stale "let g:hive_runtime_stale = exists('*getscriptinfo')")
          "a Vim without getscriptinfo() sources as before")
      (is (str/starts-with? source "if empty(g:hive_runtime_stale) | call map(")
          "nothing is sourced over an autoload script loaded from elsewhere")
      (is (< (str/index-of source "'/r/it''s/autoload/**/*.vim'")
             (str/index-of source "'/r/it''s/plugin/**/*.vim'")))
      (is (str/includes? source "execute('source ' . fnameescape(f))"))
      (is (str/includes? source "restart Vim to use ' . '/r/it''s'")))))

;; =============================================================================
;; Interpreter
;; =============================================================================

(deftest run-plan-stops-at-the-first-failure-and-marks-the-rest-skipped
  (let [fs (rb/recording-fs #{:fs/copy-tree})
        plan [{:effect :fs/delete-tree :path "/a"}
              {:effect :fs/copy-tree :from "/s" :to "/a"}
              {:effect :fs/write :path "/a/x" :content "y"}]
        outcomes (rb/run-plan! {:fs fs} plan)]
    (is (m/validate [:vector rs/Outcome] outcomes))
    (is (= [true false false] (mapv :ok? outcomes)))
    (is (= "copy failed" (:error (second outcomes))))
    (is (:skipped? (last outcomes)))
    (is (= [:fs/delete-tree] (mapv :effect @(:log fs))) "nothing after the failure ran"))
  (testing "a client eval without an injected eval-fn is a failure, not a throw"
    (is (= [false] (mapv :ok? (rb/run-plan! {:fs (rb/recording-fs)}
                                            [{:effect :client/eval :client :vim :commands ["x"]}]))))))

;; =============================================================================
;; Provisioner over a real filesystem
;; =============================================================================

(deftest provisioning-installs-renders-activates-and-deprovisioning-removes
  (let [root (temp-dir)
        src (source-dir! root)
        home (str (io/file root "home"))
        evals (atom [])]
    (try
      (reset! test-hooks {:demo/dir (str src) :demo/port-file (fn [] "/state/vim.port")})
      (let [p (rb/provisioner {:home home :eval-fn (fn [client cmds] (swap! evals conj [client cmds]))})
            spec {:addon/id "runtime.demo" :addon/runtime [(decl)]}
            report ((:provision p) spec (ok-ctor nil))
            dir (io/file home ".vim/pack/hive/start/demo")]
        (is (m/validate rs/ProvisionReport report) (pr-str (m/explain rs/ProvisionReport report)))
        (is (:ok? report))
        (is (true? (:live-ok? (first (:runtimes report)))))
        (is (.isFile (io/file dir "plugin" "demo.vim")))
        (is (.isFile (io/file dir "autoload" "demo" "deep.vim")) "nested files are copied")
        (is (str/includes? (slurp (io/file dir "plugin" "zz_hive_runtime.vim")) "'/state/vim.port'")
            "hook-bound values reach the loader")
        (is (= :vim (ffirst @evals)) "running clients are activated")
        (is (= {"runtime.demo" [(decl)]} ((:installed p))))
        (testing "reprovisioning replaces the previous install"
          (spit (io/file dir "stale.txt") "old")
          (is (:ok? ((:provision p) spec (ok-ctor nil))))
          (is (not (.exists (io/file dir "stale.txt")))))
        (testing "deprovision removes what was installed"
          (is (:ok? ((:deprovision p) "runtime.demo")))
          (is (not (.exists dir)))
          (is (nil? ((:deprovision p) "runtime.demo")) "nothing left to remove")))
      (finally
        (reset! test-hooks {})
        (delete-tree! root)))))

(deftest provisioning-degrades-per-runtime-and-never-throws
  (let [p (rb/provisioner {:home "/nonexistent-home" :fs (rb/recording-fs)})]
    (reset! test-hooks {:demo/port-file "/p"})
    (try
      (testing "no declaration, no report"
        (is (nil? ((:provision p) {:addon/id "x"} (ok-ctor nil)))))
      (testing "an absent hook fails that runtime with a reason"
        (let [report ((:provision p) {:addon/id "x" :addon/runtime [(decl)]} (ok-ctor nil))]
          (is (false? (:ok? report)))
          (is (= "addon hook :demo/dir is absent or returned nil"
                 (:error (first (:runtimes report)))))))
      (testing "an invalid declaration and an unknown client are reported, not thrown"
        (let [report ((:provision p) {:addon/id "x"
                                      :addon/runtime [(decl {:runtime/id "../escape"})
                                                      (decl {:runtime/client :acme-editor})]}
                      (ok-ctor nil))]
          (is (= [false false] (mapv :ok? (:runtimes report))))
          (is (str/starts-with? (:error (first (:runtimes report))) "invalid runtime declaration"))
          (is (= "no client profile for :acme-editor" (:error (second (:runtimes report)))))))
      (testing "host-supplied declarations apply to an addon that ships none"
        (reset! test-hooks {:demo/dir "/src" :demo/port-file "/p"})
        (let [hosted (rb/provisioner {:home "/h" :fs (rb/recording-fs) :live? false
                                      :declarations {"x" [(decl)]}})]
          (is (:ok? ((:provision hosted) {:addon/id "x"} (ok-ctor nil))))))
      (testing "a failing live activation keeps the install"
        (reset! test-hooks {:demo/dir "/src" :demo/port-file "/p"})
        (let [live-fail (rb/provisioner {:home "/h" :fs (rb/recording-fs)
                                         :eval-fn (fn [_ _] (throw (ex-info "no vim" {})))})
              rt (first (:runtimes ((:provision live-fail) {:addon/id "x" :addon/runtime [(decl)]}
                                    (ok-ctor nil))))]
          (is (true? (:ok? rt)))
          (is (false? (:live-ok? rt)))))
      (finally (reset! test-hooks {})))))

;; =============================================================================
;; Mount wiring
;; =============================================================================

(defn- spec
  [id ctor runtime]
  (cond-> {:addon/id id :addon/type :native
           :addon/init-ns "hive-addon.runtime-test" :addon/init-fn ctor}
    runtime (assoc :addon/runtime runtime)))

(deftest mount-provisions-after-init-and-teardown-deprovisions
  (let [root (temp-dir)
        src (source-dir! root)
        home (str (io/file root "home"))
        opts {:license-gate (constantly nil) :init-retry {:max-attempts 1}}]
    (try
      (reset! test-hooks {:demo/dir (str src) :demo/port-file "/state/vim.port"})
      (let [p (rb/provisioner {:home home :live? false})
            host (mount/atom-mount-host)
            report (mount/mount! (mount/solve [(spec "runtime.demo" "ok-ctor" [(decl)])
                                               (spec "runtime.failing" "failing-ctor" [(decl {:runtime/id "failing"})])])
                                 host (assoc opts :provision (:provision p)))
            by-id (into {} (map (juxt :addon/id identity)) (:mounted report))
            dir (io/file home ".vim/pack/hive/start/demo")]
        (is (true? (get-in by-id ["runtime.demo" :runtime :ok?])))
        (is (.isFile (io/file dir "plugin" "zz_hive_runtime.vim")) "injection installed the runtime")
        (is (not (contains? (get by-id "runtime.failing") :runtime)) "an addon that failed init is not provisioned")
        (is (not (.exists (io/file home ".vim/pack/hive/start/failing"))))
        (let [td (mount/teardown! host (:order report) {:deprovision (:deprovision p)})]
          (is (true? (:teardown/data-preserved? td)))
          (is (true? (get-in td [:runtime "runtime.demo" :ok?])))
          (is (not (.exists dir)) "teardown removed the runtime")))
      (testing "without :provision the mount is exactly as before"
        (let [host (mount/atom-mount-host)
              report (mount/mount! (mount/solve [(spec "runtime.demo" "ok-ctor" [(decl)])]) host opts)]
          (is (:ok? report))
          (is (not-any? #(contains? % :runtime) (:mounted report)))
          (is (not (contains? (mount/teardown! host (:order report)) :runtime)))))
      (testing "a provisioner that throws is reported and the mount still succeeds"
        (let [report (mount/mount! (mount/solve [(spec "runtime.demo" "ok-ctor" [(decl)])])
                                   (mount/atom-mount-host)
                                   (assoc opts :provision (fn [_ _] (throw (ex-info "boom" {})))))]
          (is (:ok? report))
          (is (= {:addon/id "runtime.demo" :ok? false :runtimes [] :error "boom"}
                 (:runtime (first (:mounted report)))))))
      (finally
        (reset! test-hooks {})
        (delete-tree! root)))))

;; =============================================================================
;; A real Vim
;; =============================================================================

(defn- vim-with-packages?
  "vim with +packages and +timers, and `script` to give it a terminal: without
   one Vim never reaches VimEnter, which is what a started client does."
  []
  (let [{:keys [exit out]} (sh/sh "sh" "-c" "command -v script >/dev/null && command -v vim >/dev/null && vim --version")]
    (and (zero? exit) (str/includes? out "+packages") (str/includes? out "+timers"))))

(defn- run-vim!
  "Run vim with ARGS under a pseudo-terminal, quitting shortly after VimEnter."
  [home args]
  (let [quit "call timer_start(400, {-> execute('qa!')})"
        cmd (str/join " " (map #(str "'" (str/replace % "'" "'\\''") "'")
                              (concat ["vim" "-N" "-i" "NONE"] args ["-c" quit])))]
    (sh/sh "timeout" "20" "script" "-qec" cmd "/dev/null"
           :env {"HOME" home "TERM" "xterm" "PATH" (System/getenv "PATH")})))

(defn- marker-lines
  [f]
  (when (.isFile (io/file f))
    (str/split-lines (slurp f))))

(deftest vim-loads-the-installed-runtime-at-startup-and-live
  (if-not (vim-with-packages?)
    (println "SKIP runtime vim test: no vim with +packages +timers, or no script")
    (let [root (temp-dir)
          src (source-dir! root)
          home (str (io/file root "home"))
          marker (io/file root "activated.txt")
          live-marker (io/file root "live.txt")
          on-load ["call writefile([string(get(g:, 'demo_plugin_loaded', 0)), string(v:vim_did_enter)], '{{port-file}}')"]]
      (try
        (reset! test-hooks {:demo/dir (str src) :demo/port-file (str marker)})
        (let [p (rb/provisioner {:home home :live? false})]
          (is (:ok? ((:provision p) {:addon/id "runtime.demo" :addon/runtime [(decl {:runtime/on-load on-load})]}
                     (ok-ctor nil))))
          (testing "a Vim started afterwards autoloads the runtime and runs on-load at VimEnter"
            (run-vim! home ["-u" "NORC" "--cmd" (str "set packpath=" home "/.vim")])
            (is (= ["1" "1"] (marker-lines marker))
                "the runtime's plugin loaded, then the loader ran once Vim had entered"))
          (testing "a Vim already running activates through the live commands"
            (reset! test-hooks {:demo/dir (str src) :demo/port-file (str live-marker)})
            (is (:ok? ((:provision p) {:addon/id "runtime.demo" :addon/runtime [(decl {:runtime/on-load on-load})]}
                       (ok-ctor nil))))
            (let [install-dir (rp/install-dir (:vim rp/default-profiles) "demo" home)
                  live (:commands (first (rp/live-plan (:vim rp/default-profiles) install-dir)))
                  after-enter (str "call timer_start(50, {-> execute(["
                                   (str/join ", " (map rp/vim-string live)) "])})")]
              (run-vim! home ["-u" "NONE" "-c" after-enter])
              (is (= ["1" "1"] (marker-lines live-marker))
                  "sourcing the installed plugins in a running Vim ran the loader immediately")))
          (testing "a Vim holding the runtime's autoload script from another directory is told to restart"
            ;; Vim keeps the first definition of an autoload function (E1073 on
            ;; the second), so sourcing over it would mix old and new scripts.
            (let [stale-marker (io/file root "stale.txt")
                  old-dir (io/file root "old-src")
                  install-dir (rp/install-dir (:vim rp/default-profiles) "demo" home)
                  live (:commands (first (rp/live-plan (:vim rp/default-profiles) install-dir)))
                  after-enter (str "call timer_start(50, {-> execute(["
                                   (str/join ", " (map rp/vim-string
                                                       (conj live (str "call writefile([string(g:hive_runtime_stale), string(exists('g:demo_plugin_loaded'))], "
                                                                       (rp/vim-string (str stale-marker)) ")"))))
                                   "])})")]
              (io/make-parents (io/file old-dir "autoload" "demo" "deep.vim"))
              (spit (io/file old-dir "autoload" "demo" "deep.vim") "\" an older copy\n")
              (run-vim! home ["-u" "NONE"
                              "-c" (str "source " (io/file old-dir "autoload" "demo" "deep.vim"))
                              "-c" after-enter])
              (let [[stale loaded] (marker-lines stale-marker)]
                (is (= (str "['" old-dir "/autoload/demo/deep.vim']") stale)
                    "the loaded copy is named")
                (is (= "0" loaded) "and nothing of the new install was sourced over it")))))
        (finally
          (reset! test-hooks {})
          (delete-tree! root))))))
