(ns avi.t-spec-runner
  (:require [midje.sweet :refer :all]
            [avi.spec-runner :as sr]))

(fact "it gets the lines from the file"
  (first (sr/spec-lines)) => "Normal Mode")

;; The number of tests is the number of leaf nodes
;; An indented thing without a parent is ERROR
;; Indentations are 2 spaces
;; Blank lines are either IGNORED for now

(fact "specs is a vector"
  (sr/specs []) => vector?)
(fact "it gives us zero specs when the file has no lines"
  (sr/specs []) => empty?)

(facts "about the test parameters"
  (fact "it has the initial editor contents"
    (:setup (sr/test-parameters)) => (contains {:editing "hello world"}))
  (fact "it has the keystrokes"
    (:setup (sr/test-parameters)) => (contains {:after "$b"}))
  (fact "it has the resulting point"
    (:point (sr/test-parameters)) => [0 6]))

(fact ""
  (sr/string->node "Root") => ["Root"])

(facts "about turning lines into a tree"
  (sr/lines->tree ["Root"]) => ["Root"] 
  (sr/lines->tree ["Root" "  Node"]) => ["Root"  ["Node"]]
  ) 

(facts "about indentation level"
  (sr/indent-level "hi mom") => 0
  (sr/indent-level "    i'm not your son") => 4)
