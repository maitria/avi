(ns avi.search-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding `/`"
  (fact "`/` echoes on the command line"
    (terminal :line :message :after "/") => "/")
  (fact "characters typed after `/` echo on the commmand line"
    (terminal :line :message :after "/foo") => "/foo")
  (fact "`/` commands don't execute like `:` commands"
    (:finished? (editor :after "/q<Enter>")) => falsey)
  (fact "`/xx` command jumps to the first occurrence of `xx`"
    (cursor :editing "abcxxy" :after "/xx<Enter>") => [0 3]))
