(ns hive-addon.lifecycle.store
  "A directory of EDN files as an ISurfaceStore: one `<addon-id>.edn` per addon.

   Writes go to a sibling temp file first and are moved into place, so a reader
   never sees half a surface."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-addon.lifecycle.port :as lport]
            [hive-addon.lifecycle.surface :as surface]
            [hive-dsl.result :as r])
  (:import [java.io File]
           [java.nio.file Files StandardCopyOption CopyOption]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- file-name
  "ADDON-ID as one safe path segment."
  [addon-id]
  (str (str/replace (str addon-id) #"[^A-Za-z0-9._-]" "_") ".edn"))

(defn- pinned-pr-str
  "pr-str that does not depend on the calling thread's printer bindings."
  [v]
  (binding [*print-namespace-maps* false
            *print-readably* true
            *print-dup* false
            *print-meta* false
            *print-length* nil
            *print-level* nil]
    (pr-str v)))

(defrecord EdnDirSurfaceStore [dir]
  lport/ISurfaceStore
  (-load-surface [_ addon-id]
    (let [f (io/file dir (file-name addon-id))]
      (when (.isFile f)
        (r/rescue nil (surface/normalize (edn/read-string (slurp f)))))))
  (-save-surface! [_ addon-id s]
    (r/rescue nil
      (let [d   (io/file dir)
            f   (io/file d (file-name addon-id))
            tmp (io/file d (str "." (file-name addon-id) ".tmp"))]
        (.mkdirs d)
        (spit tmp (pinned-pr-str (surface/normalize s)))
        (Files/move (.toPath ^File tmp) (.toPath ^File f)
                    (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING
                                            StandardCopyOption/ATOMIC_MOVE]))))
    nil))

(defn edn-dir-store
  "An ISurfaceStore persisting under DIR (created on first save)."
  [dir]
  (->EdnDirSurfaceStore (str dir)))
