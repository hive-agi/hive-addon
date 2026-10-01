(ns hive-addon.plug.source
  "Plug sources as trust-aware value objects: a tools.deps coord classified into a
   family with its mutability/locality, behind the ISource protocol.")

;; SPDX-License-Identifier: MIT

(defprotocol ISource
  (family   [_] "Coord family: :local | :git | :mvn.")
  (local?   [_] "Dev-only filesystem source?")
  (mutable? [_] "Can resolve to a different artifact over time?"))

(defn- valid-sha? [sha]
  (boolean (re-matches #"[0-9a-fA-F]{7,40}" (str sha))))

(defn- mutable-version? [v]
  (let [s (str v)]
    (boolean (or (re-find #"(?i)snapshot" s)
                 (re-matches #"(?i)\s*(latest|release)\s*" s)
                 (re-find #"[\[\](),]" s)))))

(defrecord LocalRoot [root]
  ISource
  (family   [_] :local)
  (local?   [_] true)
  (mutable? [_] true))

(defrecord GitDep [url sha tag]
  ISource
  (family   [_] :git)
  (local?   [_] false)
  ;; `(:sha this)`, not the bare field symbol `sha`: cljrs does not bind a
  ;; defrecord's fields as symbols inside its method bodies (it answers
  ;; `unbound symbol: sha`), while keyword access on `this` works everywhere.
  (mutable? [this] (not (valid-sha? (:sha this)))))

(defrecord MvnDep [version]
  ISource
  (family   [_] :mvn)
  (local?   [_] false)
  (mutable? [this] (mutable-version? (:version this))))

(defn coord->source
  "ISource value object for a tools.deps coord, or nil for an unknown coord."
  [coord]
  (cond
    (contains? coord :local/root)  (->LocalRoot (:local/root coord))
    (contains? coord :git/url)     (->GitDep (:git/url coord) (:git/sha coord) (:git/tag coord))
    (contains? coord :mvn/version) (->MvnDep (:mvn/version coord))
    :else                          nil))

(defn families
  "Coord families a coord map declares — usually one; more than one is ambiguous."
  [coord]
  (cond-> #{}
    (contains? coord :local/root)  (conj :local)
    (contains? coord :git/url)     (conj :git)
    (contains? coord :mvn/version) (conj :mvn)))
