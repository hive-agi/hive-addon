(ns hive-addon.plug-test
  "Golden + generative tests for the iaddon.edn schema layer and pure resolver."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.set :as set]
            [malli.generator :as mg]
            [hive-addon.plug :as p]
            [hive-addon.plug.schema :as ps]
            [hive-dsl.result :as r]))

(def A 'org/a)
(def B 'org/b)
(def C 'org/c)

;; =============================================================================
;; Schema — golden
;; =============================================================================

(deftest source-families-validate
  (is (ps/validate ps/Source {:local/root "../x"}))
  (is (ps/validate ps/Source {:git/url "https://g" :git/sha "abcdef1"}))
  (is (ps/validate ps/Source {:mvn/version "0.2.0"}))
  (is (not (ps/validate ps/Source {:nonsense 1})))
  (is (not (ps/validate ps/MvnCoord {:mvn/version ""}))))

(deftest cred-step-is-reference-shape
  (is (ps/validate ps/CredStep [:env "CLOJARS_PASSWORD"]))
  (is (ps/validate ps/CredStep [:pass "Github/clojars-token"]))
  (is (ps/validate ps/CredStep [:literal])))

(deftest full-config-validates
  (is (ps/validate ps/IaddonConfig
        {:iaddon/plugs {A {:source {:mvn/version "0.2.0"} :capability :cartography :class :native}}
         :iaddon/trust {:require-signature #{:proprietary :external} :allow-unsigned #{:native :foss}}
         :iaddon/capabilities {:cartography {:prefer A}}}))
  (is (ps/validate :iaddon/config {:iaddon/plugs {}}))
  (is (r/ok? (ps/validate* ps/IaddonConfig {:iaddon/plugs {}})))
  (is (r/err? (ps/validate* ps/IaddonConfig {:iaddon/plugs {A {}}})))) ; missing :source

;; =============================================================================
;; Resolver — units
;; =============================================================================

(deftest deep-merge-recurses
  (is (= {:a {:b 1 :c 2}} (p/deep-merge {:a {:b 1}} {:a {:c 2}})))
  (is (= {:a 2} (p/deep-merge {:a 1} {:a 2}))))

(deftest trust-security-inversion
  (testing "require-signature UNIONs, allow-unsigned INTERSECTs then drops required"
    (is (= {:require-signature #{:proprietary :external} :allow-unsigned #{:native :foss}}
           (:iaddon/trust
            (p/merge-configs
             [{:iaddon/trust {:require-signature #{:proprietary} :allow-unsigned #{:native :foss :proprietary}}}
              {:iaddon/trust {:require-signature #{:external}    :allow-unsigned #{:native :foss :proprietary :external}}}]))))))

(deftest source-family-classifies
  (is (= :local (p/source-family {:local/root ".."})))
  (is (= :git   (p/source-family {:git/url "u"})))
  (is (= :mvn   (p/source-family {:mvn/version "1"})))
  (is (nil? (p/source-family {:x 1}))))

(deftest profile-filtering
  (let [cfg {:iaddon/plugs {A {:source {:mvn/version "1"}} B {:source {:mvn/version "1"}} C {:source {:mvn/version "1"}}}
             :iaddon/profiles {:min {:only #{A}} :noB {:except #{B}}}}]
    (is (= #{A B C} (:kept (p/apply-profile cfg nil))))
    (is (= {:kept #{A} :excluded {B :not-in-profile-only C :not-in-profile-only}} (p/apply-profile cfg :min)))
    (is (= {:kept #{A C} :excluded {B :profile-excepted}} (p/apply-profile cfg :noB)))))

(deftest disabled-plugs-drop
  (is (= {:kept #{B} :dropped {A :disabled}}
         (p/drop-disabled {A {:enabled? false} B {}} #{A B}))))

(deftest capability-contention
  (let [ok       {:iaddon/plugs {A {:source {:mvn/version "1"} :capability :x}
                                 B {:source {:mvn/version "1"} :capability :y}}}
        prefer   {:iaddon/plugs {A {:source {:mvn/version "1"} :capability :x}
                                 B {:source {:mvn/version "1"} :capability :x}}
                  :iaddon/capabilities {:x {:prefer A}}}
        hard     {:iaddon/plugs {A {:source {:mvn/version "1"} :capability :x}
                                 B {:source {:mvn/version "1"} :capability :x}}}]
    (is (= #{A B} (:selected (:ok (p/select-capabilities ok #{A B})))))
    (is (= #{A} (:selected (:ok (p/select-capabilities prefer #{A B})))))
    (is (= {:capability-lost-to A :capability :x} (get-in (p/select-capabilities prefer #{A B}) [:ok :dropped B])))
    (is (r/err? (p/select-capabilities hard #{A B})))
    (is (= :iaddon/capability-conflict (:error (p/select-capabilities hard #{A B}))))))

(deftest lint-fail-closed
  (testing "literal secret in a credential chain"
    (is (= :iaddon/lint-failed (:error (p/lint {:iaddon/credentials {:h {:chain [[:literal "tok"]]}}})))))
  (testing "inline secret on a plug"
    (is (= :iaddon/lint-failed (:error (p/lint {:iaddon/plugs {A {:source {:mvn/version "1"} :config {:password "x"}}}})))))
  (testing "proprietary plug on :local/root"
    (is (= :source-family-mismatch
           (:rule (first (:violations (p/lint {:iaddon/plugs {A {:class :proprietary :source {:local/root ".."}}}})))))))
  (testing "proprietary plug on mutable git tag"
    (is (= :mutable-tag
           (:rule (first (:violations (p/lint {:iaddon/plugs {A {:class :proprietary :source {:git/url "u" :git/tag "v1"}}}})))))))
  (testing "clean config passes"
    (is (r/ok? (p/lint {:iaddon/plugs {A {:class :proprietary :source {:mvn/version "1"}}}})))))

;; =============================================================================
;; Orchestration — end to end
;; =============================================================================

(def layers-happy
  [{:id :defaults
    :config {:iaddon/plugs {A {:source {:mvn/version "0.2.0"} :capability :cartography :class :native}
                            B {:source {:mvn/version "0.1.0"} :capability :cartography :class :native}
                            C {:source {:mvn/version "1"} :enabled? false :class :native}}}}
   {:id :project
    :config {:iaddon/plugs {A {:addon/id "carto"}}
             :iaddon/capabilities {:cartography {:prefer A}}
             :iaddon/trust {:require-signature #{:proprietary}}}}])

(deftest resolve-happy-path
  (let [res (p/resolve-config layers-happy {:profile nil})]
    (is (r/ok? res))
    (is (= #{A} (set (keys (:selected (:ok res))))))
    (is (= {:source {:mvn/version "0.2.0"} :capability :cartography :class :native :addon/id "carto"}
           (get-in res [:ok :selected A])))
    (is (= {B {:capability-lost-to A :capability :cartography} C :disabled} (:dropped (:ok res))))
    (is (= #{:proprietary} (get-in res [:ok :config :iaddon/trust :require-signature])))))

(deftest resolve-provenance
  (let [prov (p/plug-provenance layers-happy)]
    (is (= :project  (get-in prov [A ::p/layer])))
    (is (= :project  (get-in prov [A :addon/id])))
    (is (= :defaults (get-in prov [A :source])))
    (is (= :defaults (get-in prov [B ::p/layer])))))

(deftest resolve-error-paths
  (testing "capability conflict with no :prefer is a hard error"
    (is (= :iaddon/capability-conflict
           (:error (p/resolve-config (update-in layers-happy [1 :config] dissoc :iaddon/capabilities) {})))))
  (testing "lint failure short-circuits"
    (is (= :iaddon/lint-failed
           (:error (p/resolve-config [{:id :d :config {:iaddon/plugs {A {:class :proprietary :source {:local/root "../x"}}}}} ] {})))))
  (testing "nil section is a fail-closed schema violation"
    (is (= :iaddon/schema-violation
           (:error (p/resolve-config [{:id :d :config {:iaddon/plugs {A {:source {:mvn/version "1"}}} :iaddon/capabilities nil}}] {}))))))

(deftest explain-shape
  (is (= :ok (:status (p/explain layers-happy {}))))
  (is (= #{A} (set (:selected (p/explain layers-happy {})))))
  (is (= :error (:status (p/explain [{:id :d :config {:iaddon/plugs {A {:class :proprietary :source {:local/root "../x"}}}}}] {})))))

;; =============================================================================
;; Generative — schema <-> resolver invariants
;; =============================================================================

(deftest gen-source-family-total
  (testing "every schema-valid Source classifies to a known family (never :unknown)"
    (doseq [s (mg/sample (ps/schema ps/Source) {:size 30 :seed 42})]
      (is (contains? #{:local :git :mvn} (p/source-family s)) (str "unclassified: " s)))))

(deftest gen-generated-plug-validates
  (doseq [pl (mg/sample (ps/schema ps/Plug) {:size 25 :seed 7})]
    (is (ps/validate ps/Plug pl))))

(deftest gen-trust-inversion-invariants
  (testing "merged require ⊇ each layer's require; merged allow is disjoint from require"
    (doseq [t1 (mg/sample (ps/schema ps/Trust) {:size 12 :seed 3})
            t2 (mg/sample (ps/schema ps/Trust) {:size 12 :seed 9})]
      (let [m   (:iaddon/trust (p/merge-configs [{:iaddon/trust t1} {:iaddon/trust t2}]))
            req (set (:require-signature m))
            aus (set (:allow-unsigned m))]
        (is (set/subset? (set (:require-signature t1)) req))
        (is (set/subset? (set (:require-signature t2)) req))
        (is (empty? (set/intersection req aus)))))))

(deftest gen-resolve-never-throws
  (testing "resolve-config always returns a Result (ok or err), never throws"
    (doseq [cfg (mg/sample (ps/schema ps/IaddonConfig) {:size 20 :seed 5})]
      (let [res (p/resolve-config [{:id :gen :config cfg}] {})]
        (is (or (r/ok? res) (r/err? res)))))))

;; =============================================================================
;; Hardening — adversarial-review findings (all live-confirmed on 7922)
;; =============================================================================

(deftest hardening-trust-critical-merge
  (testing "keyring is first(lowest)-wins — a higher layer cannot swap the trust root"
    (is (= "/org/trusted.gpg"
           (get-in (p/merge-configs [{:iaddon/trust {:keyring "/org/trusted.gpg" :require-signature #{:proprietary}}}
                                     {:iaddon/trust {:keyring "/tmp/attacker.gpg"}}])
                   [:iaddon/trust :keyring]))))
  (testing "allow-unsigned omission is fail-closed — a higher layer cannot seed a grant"
    (is (nil? (get-in (p/merge-configs [{:iaddon/trust {:require-signature #{:external}}}
                                        {:iaddon/trust {:allow-unsigned #{:proprietary}}}])
                      [:iaddon/trust :allow-unsigned]))))
  (testing "a lower layer that DOES grant seeds the ceiling; a higher layer can only shrink it"
    (is (= #{:foss}
           (get-in (p/merge-configs [{:iaddon/trust {:allow-unsigned #{:foss}}}
                                     {:iaddon/trust {:allow-unsigned #{:foss :proprietary}}}])
                   [:iaddon/trust :allow-unsigned]))))
  (testing "pinned is lower-wins on conflict; a higher layer may ADD but not repin/drop"
    (is (= {"addonA" "goodsha" "addonB" "goodshaB" "addonC" "newpin"}
           (get-in (p/merge-configs [{:iaddon/trust {:pinned {"addonA" "goodsha" "addonB" "goodshaB"}}}
                                     {:iaddon/trust {:pinned {"addonA" "ATTACKERSHA" "addonC" "newpin"}}}])
                   [:iaddon/trust :pinned])))))

(deftest hardening-lint-mvn-mutable
  (doseq [v ["1.0-SNAPSHOT" "LATEST" "RELEASE" "[1.0,2.0)" "1.0.0-snapshot"]]
    (is (r/err? (p/lint {:iaddon/plugs {A {:class :proprietary :source {:mvn/version v}}}})) (str "mutable: " v)))
  (is (r/ok? (p/lint {:iaddon/plugs {A {:class :proprietary :source {:mvn/version "1.2.3"}}}}))))

(deftest hardening-lint-git-sha-hex
  (testing "blank/whitespace/non-hex sha does not satisfy the pin"
    (is (r/err? (p/lint {:iaddon/plugs {A {:class :proprietary :source {:git/url "u" :git/sha "       "}}}})))
    (is (r/ok?  (p/lint {:iaddon/plugs {A {:class :proprietary :source {:git/url "u" :git/sha "abcdef0123"}}}})))))

(deftest hardening-lint-nested-and-nonstring-secrets
  (is (r/err? (p/lint {:iaddon/plugs {A {:source {:mvn/version "1"} :config {:db {:password "hunter2"}}}}})))
  (is (r/err? (p/lint {:iaddon/plugs {A {:source {:mvn/version "1"} :config {:password 12345}}}})))
  (is (r/err? (p/lint {:iaddon/plugs {A {:source {:mvn/version "1"} :config {:pass "x"}}}})))
  (is (r/err? (p/lint {:iaddon/plugs {A {:source {:mvn/version "1"} :config {:aws {:access-token "x"}}}}})))
  (testing "a :credential HANDLE (keyword) is a reference, not a secret"
    (is (r/ok? (p/lint {:iaddon/plugs {A {:source {:mvn/version "1"} :credential :clojars}}})))))

(deftest hardening-lint-cred-args
  (testing "a reference step with the var NAME only is clean (no false positive)"
    (is (r/ok? (p/lint {:iaddon/credentials {:h {:chain [[:env "CLOJARS_PASSWORD"]]}}}))))
  (testing "excess args smuggle a literal"
    (is (r/err? (p/lint {:iaddon/credentials {:h {:chain [[:env "CLOJARS_PASSWORD" "actual-secret"]]}}}))))
  (testing "map arg with an inline :default is rejected"
    (is (r/err? (p/lint {:iaddon/credentials {:h {:chain [[:env {:default "s3cr3t"}]]}}})))))

(deftest hardening-lint-class-default-strict
  (testing "a plug with NO :class is held to the strict posture (fail-closed default)"
    (is (r/err? (p/lint {:iaddon/plugs {A {:source {:local/root "/tmp/evil"}}}})))
    (is (r/err? (p/lint {:iaddon/plugs {A {:source {:mvn/version "9-SNAPSHOT"}}}}))))
  (testing "an explicit :native/:foss plug may use dev sources"
    (is (r/ok? (p/lint {:iaddon/plugs {A {:class :native :source {:local/root "../sib"}}}})))
    (is (r/ok? (p/lint {:iaddon/plugs {A {:class :foss :source {:local/root "../sib"}}}})))))

(deftest hardening-lint-ambiguous-source
  (is (= :ambiguous-source
         (:rule (first (:violations (p/lint {:iaddon/plugs {A {:class :native :source {:local/root "x" :mvn/version "1"}}}})))))))

(deftest hardening-lint-repo-url-userinfo
  (is (r/err? (p/lint {:iaddon/repos {:priv {:url "https://user:s3cr3t@repo.internal/maven"}}
                       :iaddon/plugs {A {:class :native :source {:mvn/version "1"}}}})))
  (is (r/ok? (p/lint {:iaddon/repos {:pub {:url "https://repo.internal/maven"}}
                      :iaddon/plugs {A {:class :native :source {:mvn/version "1"}}}}))))

(deftest hardening-permit-enforced
  (let [cfg {:iaddon/plugs {A {:source {:mvn/version "1"} :capability :search}}
             :iaddon/capabilities {:search {:permit false}}}
        res (p/select-capabilities cfg #{A})]
    (is (r/ok? res))
    (is (= #{} (:selected (:ok res))))
    (is (= {:capability-permit-denied [:search]} (get-in res [:ok :dropped A])))))

(deftest hardening-plural-capabilities-contend
  (let [cfg {:iaddon/plugs {A {:source {:mvn/version "1"} :capabilities #{:search}}
                            B {:source {:mvn/version "1"} :capabilities #{:search}}}}]
    (is (r/err? (p/select-capabilities cfg #{A B})))
    (is (= :iaddon/capability-conflict (:error (p/select-capabilities cfg #{A B}))))))

(deftest hardening-prefer-unavailable
  (testing ">1 provider with :prefer naming a lib not among them is an explicit conflict"
    (let [cfg {:iaddon/plugs {A {:source {:mvn/version "1"} :capability :x}
                              B {:source {:mvn/version "1"} :capability :x}}
               :iaddon/capabilities {:x {:prefer C}}}
          res (p/select-capabilities cfg #{A B})]
      (is (r/err? res))
      (is (= C (:prefer-unavailable (first (:conflicts res))))))))

(deftest hardening-empty-layers-clean
  (is (= :ok (:status (p/explain [] {}))))
  (is (= [] (:selected (p/explain [] {}))))
  (is (= :ok (:status (p/explain [{:id :a :config nil}] {}))))
  (is (= {} (p/merge-configs []))))
