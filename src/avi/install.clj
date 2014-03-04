(ns avi.install
  (:require [clojure.java.io :as io]
            [clojure.java.shell]))

(defn- sh
  [& args]
  (let [result (apply clojure.java.shell/sh args)]
    (if-not (= "" (:err result))
      (println (:err result)))))

(defn- version
  []
  (nth (read-string (slurp "project.clj")) 2))

(defn install-commands
  [prefix get-property]
  (let [os-name (get-property "os.name")
        include-path "/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers" 
        avi-bin-path (str prefix "/bin/avi")
        avi-jar-dir (str prefix "/share/avi/")
        avi-jar-path (str avi-jar-dir "/avi.jar")
        avi-lib-dir (str prefix "/lib/")
        dll-suffix (if (= os-name "Mac OS X")
                     ".dylib"
                     ".so")
        avi-Screen-path (str avi-lib-dir "libavi_jni" dll-suffix)]
    [["install" "-d" avi-jar-dir]
     ["install" "-m" "0755" "bin/avi" avi-bin-path]
     ["install" "-m" "0644" (str "target/avi-" (version) "-standalone.jar") avi-jar-path]
     ["install" "-d" avi-lib-dir]
     (concat
       ["cc"]
       (if (= os-name "Linux")
         ["-fPIC"])
       ["-shared"
        (str "-I" include-path)
        (str "-I" (str include-path "/darwin"))
        "-o" avi-Screen-path
        "c-src/screen.c"
        "-lcurses"] 
       (if (= os-name "Linux")
         ["-ltinfo"]))]))

(defn install
  []
  (doseq [cmd (install-commands "/usr/local" #(System/getProperty %))]
    (apply sh cmd)))
