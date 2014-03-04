(ns avi.install-test
  (:require [midje.sweet :refer :all]
            [avi.install :refer :all]))

(def test-prefix "/foo")

(defn build-command
  [os-name]
  (last (install-commands test-prefix os-name)))

(facts "about installing avi"
  (facts "about installing on Mac OS X"
    (fact "the JNI library name is libavi.dylib"
      (build-command "Mac OS X") => (contains ["-o" "/foo/lib/libavi_jni.dylib"]))
    (fact "the build command has -lcurses"
      (build-command "Mac OS X") => (contains ["-lcurses"]))))
