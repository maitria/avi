(ns avi.install
  (:require [clojure.java.io :as io]
            [clojure.java.shell]))

(def prefix "/usr/local/")
(def avi-bin-path (str prefix "bin/avi"))
(def avi-jar-dir (str prefix "share/avi/"))
(def avi-jar-path (str avi-jar-dir "avi.jar"))
(def avi-lib-dir (str prefix "lib/avi/"))
(def avi-Screen-path (str avi-lib-dir "libavi_terminal_Screen.dylib"))

(defn sh
  [& args]
  (let [result (apply clojure.java.shell/sh args)]
    (if-not (= "" (:err result))
      (println (:err result)))))

(defn- version
  []
  (nth (read-string (slurp "project.clj")) 2))

(defn install
  []
  (sh "install" "-d" avi-jar-dir)
  (sh "install" "-m" "0755" "bin/avi" avi-bin-path)
  (sh "install" "-m" "0644" (str "target/avi-" (version) "-standalone.jar") avi-jar-path)
  (sh "install" "-d" avi-lib-dir)
  (sh "cc"
      "-dynamiclib"
      (str "-I" (System/getProperty "java.home") "/../include")
      (str "-I" (System/getProperty "java.home") "/../include/darwin")
      "-lcurses"
      "-o" avi-Screen-path
      "c-src/screen.c"))
