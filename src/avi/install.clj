(ns avi.install
  (:require [clojure.java.io :as io]
            [clojure.java.shell]))

(def possible-header-locations
  ["/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers"
   (str (System/getProperty "java.home") "/../include")])

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

(defn- header-location
  []
  (->> possible-header-locations
       (filter #(.exists (io/as-file (str % "/jni.h"))))
       first))

(defn install
  []
  (sh "install" "-d" avi-jar-dir)
  (sh "install" "-m" "0755" "bin/avi" avi-bin-path)
  (sh "install" "-m" "0644" (str "target/avi-" (version) "-standalone.jar") avi-jar-path)
  (sh "install" "-d" avi-lib-dir)
  (let [include-path (header-location)]
    (sh "cc"
        "-dynamiclib"
        (str "-I" include-path)
        (str "-I" (str include-path "/darwin"))
        "-lcurses"
        "-o" avi-Screen-path
        "c-src/screen.c")))
