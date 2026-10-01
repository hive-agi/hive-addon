(ns hive-addon.runtime.schema
  "Value objects for CLIENT RUNTIMES: the editor-side code an addon ships (a
   Vim runtime directory, and later elisp) and what a host does to make a
   client load it when the addon is injected.

   A RuntimeDecl is data an addon carries in its mount manifest under
   :addon/runtime, or that a host supplies for an addon that does not. A
   ClientProfile says where a client kind autoloads code from. An Effect is one
   step of a provisioning plan; plans are pure data run by
   hive-addon.runtime.boundary.

   Rationale lives in hive memory (KG-linked), not here."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def RuntimeId
  "A directory-safe name: it becomes one path segment under the client's
   install root, so it can never contain a separator or start with a dot."
  [:re #"^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$"])

(def ClientKind
  "Which client loads the runtime. Open: a new client kind is a new profile."
  :keyword)

(def Source
  "Where the runtime files come from. :source/dir is an absolute directory;
   :source/hook names a hook of the injected addon whose value (or zero-arg fn
   result) is that directory, known only after initialize!."
  [:or
   [:map {:closed true} [:source/dir [:string {:min 1}]]]
   [:map {:closed true} [:source/hook :qualified-keyword]]])

(def Binding
  "A value substituted for {{name}} in on-load commands: a literal, or a hook
   of the injected addon resolved after initialize!."
  [:or
   [:map {:closed true} [:binding/value :string]]
   [:map {:closed true} [:binding/hook :qualified-keyword]]])

(def BindingKey
  "The name a {{placeholder}} refers to: an unqualified lower-case keyword."
  [:and :keyword [:fn {:error/message "unqualified lower-case kebab name"}
                  #(and (nil? (namespace %))
                        (boolean (re-matches #"[a-z][a-z0-9-]*" (name %))))]])

(def RuntimeDecl
  "One client runtime of one addon. :runtime/on-load are client commands (Vim
   Ex commands for :vim and :nvim) run once the client has loaded the runtime:
   at startup for a client started later, immediately for one already running."
  [:map {:closed true}
   [:runtime/id RuntimeId]
   [:runtime/client ClientKind]
   [:runtime/source Source]
   [:runtime/bindings {:optional true} [:map-of BindingKey Binding]]
   [:runtime/on-load {:optional true} [:vector :string]]])

(def RuntimeDecls
  [:vector RuntimeDecl])

(def ClientProfile
  "Where a client kind autoloads runtimes from, where the generated loader sits
   inside an installed runtime, and which loader kind renders it (the dispatch
   value of hive-addon.runtime.plan/loader-source). :profile/install-root may
   start with ~/."
  [:map {:closed true}
   [:profile/client ClientKind]
   [:profile/install-root [:string {:min 1}]]
   [:profile/loader-path [:string {:min 1}]]
   [:profile/loader :keyword]])

(def Effect
  "One provisioning step. Filesystem effects are run through IRuntimeFs;
   :client/eval sends commands to clients that are already running."
  [:multi {:dispatch :effect}
   [:fs/delete-tree [:map {:closed true}
                     [:effect [:= :fs/delete-tree]]
                     [:path [:string {:min 1}]]]]
   [:fs/copy-tree [:map {:closed true}
                   [:effect [:= :fs/copy-tree]]
                   [:from [:string {:min 1}]]
                   [:to [:string {:min 1}]]]]
   [:fs/write [:map {:closed true}
               [:effect [:= :fs/write]]
               [:path [:string {:min 1}]]
               [:content :string]]]
   [:client/eval [:map {:closed true}
                  [:effect [:= :client/eval]]
                  [:client ClientKind]
                  [:commands [:vector :string]]]]])

(def Plan
  [:vector Effect])

(def Outcome
  "What running one effect did. :skipped? marks effects not run because an
   earlier one failed."
  [:map {:closed true}
   [:effect :keyword]
   [:ok? :boolean]
   [:skipped? {:optional true} :boolean]
   [:error {:optional true} :string]])

(def RuntimeReport
  "Outcome of provisioning one runtime. :ok? is the install; :live-ok? is
   present only when activation in running clients was attempted."
  [:map {:closed true}
   [:runtime/id :string]
   [:runtime/client ClientKind]
   [:ok? :boolean]
   [:live-ok? {:optional true} :boolean]
   [:dir {:optional true} :string]
   [:outcomes [:vector Outcome]]
   [:error {:optional true} :string]])

(def ProvisionReport
  "Outcome of provisioning (or deprovisioning) every runtime of one addon.
   :error is set when the provisioner itself failed before any runtime ran."
  [:map {:closed true}
   [:addon/id :string]
   [:ok? :boolean]
   [:runtimes [:vector RuntimeReport]]
   [:error {:optional true} :string]])

(defn valid-decl?
  [x]
  (m/validate RuntimeDecl x))

(defn explain-decl
  [x]
  (m/explain RuntimeDecl x))
