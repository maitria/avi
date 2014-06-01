(ns avi.install
  (:require [clojure.java.io :as io]
            [clojure.java.shell]
            [midje.sweet :refer [facts fact contains]]))

(defn- sh
  [& args]
  (let [result (apply clojure.java.shell/sh args)]
    (if-not (= "" (:err result))
      (println (:err result)))))

(defn- version
  []
  (nth (read-string (slurp "project.clj")) 2))

(defn- build-command
  [prefix get-property]
  (let [os-name (get-property "os.name")
        include-path (if (= os-name "Mac OS X")
                       "/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers"
                       (str (get-property "java.home") "/../include"))
        avi-lib-dir (str prefix "/lib/")
        java-arch-name (if (= os-name "Mac OS X")
                         "darwin"
                         "linux")
        dll-suffix (if (= os-name "Mac OS X")
                     ".dylib"
                     ".so")
        avi-Terminal-path (str avi-lib-dir "libavi_jni" dll-suffix)]
    (concat
      ["cc"]
      (if (= os-name "Linux")
        ["-fPIC"])
      ["-shared"
       (str "-I" include-path)
       (str "-I" (str include-path "/" java-arch-name))
       "-o" avi-Terminal-path
       "c-src/terminal.c"
       "-lcurses"] 
      (if (= os-name "Linux")
        ["-ltinfo"]))))

(let [make-build-command (fn [os-name]
                          (build-command "/foo" {"os.name" os-name,
                                                 "java.home" "/java-home/jre"}))]
  (facts "about installing avi"
    (facts "about installing on Mac OS X"
      (fact "the JNI library name is libavi_jni.dylib"
        (make-build-command "Mac OS X") => (contains ["-o" "/foo/lib/libavi_jni.dylib"]))
      (fact "the build command has -lcurses"
        (make-build-command "Mac OS X") => (contains ["-lcurses"]))
      (fact "the build command does not have -ltinfo"
        (make-build-command "Mac OS X") =not=> (contains ["-ltinfo"]))
      (fact "the build command includes headers from JavaVM.framework"
        (make-build-command "Mac OS X") => (contains ["-I/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers"])
        (make-build-command "Mac OS X") => (contains ["-I/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers/darwin"]))
      (fact "the build command specifies -shared"
        (make-build-command "Mac OS X") => (contains ["-shared"]))
      (fact "the build command does not have -fPIC"
        (make-build-command "Mac OS X") =not=> (contains ["-fPIC"])))
    (facts "about installing on Linux"
      (fact "the JNI library name is libavi_jni.so"
        (make-build-command "Linux") => (contains ["-o" "/foo/lib/libavi_jni.so"]))
      (fact "the build command specifies -shared"
        (make-build-command "Linux") => (contains ["-shared"]))
      (fact "the build command specifies -ltinfo"
        (make-build-command "Linux") => (contains ["-ltinfo"]))
      (fact "the build command has -fPIC"
        (make-build-command "Linux") => (contains ["-fPIC"]))
      (fact "the build command contains headers from the java.home property"
        (make-build-command "Linux") => (contains ["-I/java-home/jre/../include"])
        (make-build-command "Linux") => (contains ["-I/java-home/jre/../include/linux"])))))

(defn install-commands
  [prefix get-property]
  (let [os-name (get-property "os.name")
        avi-bin-path (str prefix "/bin/avi")
        avi-jar-dir (str prefix "/share/avi/")
        avi-jar-path (str avi-jar-dir "/avi.jar")
        avi-lib-dir (str prefix "/lib/")]
    [["install" "-d" avi-jar-dir]
     ["install" "-m" "0755" "bin/avi" avi-bin-path]
     ["install" "-m" "0644" (str "target/avi-" (version) "-standalone.jar") avi-jar-path]
     ["install" "-d" avi-lib-dir]
     (build-command prefix get-property)]))

(defn install
  []
  (doseq [cmd (install-commands "/usr/local" #(System/getProperty %))]
    (apply sh cmd)))
