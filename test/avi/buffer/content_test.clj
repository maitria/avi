(ns avi.buffer.content-test
  (:require [midje.sweet :refer :all]
            [schema.core :as s]
            [avi.buffer.content :as c]))

(s/set-fn-validation! true)

(facts "about buffer contents"
  (fact "we can retrieve buffer contents initial text"
    (:lines (c/content "Hello, World!")) => ["Hello, World!"]
    (:lines (c/content "Line 1\nLine 2")) => ["Line 1" "Line 2"]
    (:lines (c/content "Line 1\nLine 3\n")) => ["Line 1" "Line 3"]
    (:lines (c/content "Line 1\n\nLine 3")) => ["Line 1" "" "Line 3"])
  (fact "we always have at least one line"
    (:lines (c/content "")) => [""])
  (fact "if the last character is a newline, it does not make an extra line"
    (:lines (c/content "\n")) => [""]
    (:lines (c/content "\n\n")) => ["" ""]
    (:lines (c/content "\nfoo")) => ["" "foo"]
    (:lines (c/content "foo\n")) => ["foo"]))

(facts "about replacing contents"
  (fact "replace can insert at beginning of buffer"
    (:lines (c/replace (c/content "Hello!") [1 0] [1 0] "xyz")) => ["xyzHello!"])
  (fact "replace can insert within a line"
    (:lines (c/replace (c/content "Hello!") [1 2] [1 2] "//")) => ["He//llo!"])
  (fact "replace can insert at the end of a line"
    (:lines (c/replace (c/content "Hello!") [1 6] [1 6] "//")) => ["Hello!//"]))
