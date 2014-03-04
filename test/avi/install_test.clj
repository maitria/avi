(ns avi.install-test
  (:require [midje.sweet :refer :all]
            [avi.install :refer :all]))

(def test-prefix "/foo")

(defn build-command
  [os-name]
  (last (install-commands test-prefix {"os.name" os-name})))

(facts "about installing avi"
  (facts "about installing on Mac OS X"
    (fact "the JNI library name is libavi_jni.dylib"
      (build-command "Mac OS X") => (contains ["-o" "/foo/lib/libavi_jni.dylib"]))
    (fact "the build command has -lcurses"
      (build-command "Mac OS X") => (contains ["-lcurses"]))
    (fact "the build command does not have -ltinfo"
      (build-command "Mac OS X") =not=> (contains ["-ltinfo"]))
    (fact "the build command includes headers from JavaVM.framework"
      (build-command "Mac OS X") => (contains ["-I/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers"])
      (build-command "Mac OS X") => (contains ["-I/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers/darwin"]))
    (fact "the build command specifies -shared"
      (build-command "Mac OS X") => (contains ["-shared"]))
    (fact "the build command does not have -fPIC"
      (build-command "Mac OS X") =not=> (contains ["-fPIC"])))
  (facts "about installing on Linux"
    (fact "the JNI library name is libavi_jni.so"
      (build-command "Linux") => (contains ["-o" "/foo/lib/libavi_jni.so"]))
    (fact "the build command specifies -shared"
      (build-command "Linux") => (contains ["-shared"]))
    (fact "the build command specifies -ltinfo"
      (build-command "Linux") => (contains ["-ltinfo"]))
    (fact "the build command has -fPIC"
      (build-command "Linux") => (contains ["-fPIC"]))))
