(ns viv.install
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]))

(def prefix "/usr/local/")
(def viv-bin-path (str prefix "bin/viv"))
(def viv-jar-dir (str prefix "share/viv/"))
(def viv-jar-path (str viv-jar-dir "viv.jar"))

(defn- version
  []
  (nth (read-string (slurp "project.clj")) 2))

(defn install
  []
  (sh "install" "-d" viv-jar-dir)
  (sh "install" "-m" "0755" "bin/viv" viv-bin-path)
  (sh "install" "-m" "0644" (str "target/viv-" (version) "-standalone.jar") viv-jar-path))
