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
  (fact "`/xx` jumps to the first occurrence of `xx`"
    (cursor :editing "abcxxy" :after "/xx<Enter>") => [0 3])
  (fact "`/xx` shows error when not found"
    (terminal :line :message :editing "abcyyz" :after "/xx<Enter>") => ["Did not find `xx`." :white :on :red])
  (fact "`/xx` finds an occurrence on a later line"
    (cursor :editing "abcyyz\nll\nfooxxy\nz" :after "/xx<Enter>") => [2 3])
  (fact "`/xx` finds a later occurrence on the current line"
    (cursor :editing "axxbxx" :after "ll/xx<Enter>") => [0 4])
  (fact "`/` won't find the occurrence the cursor is on"
    (cursor :editing "axxbxx" :after "l/xx<Enter>") => [0 4]))

(facts "regarding `?`"
  (fact "`?` echoes on the command line"
    (terminal :line :message :after "?") => "?")
  (fact "`?xx` jumps to the previous occurrence of `xx` on the line"
    (cursor :editing "axxbzz" :after "$?xx<Enter>") => [0 1]
    (cursor :editing "axxbxx" :after "$?xx<Enter>") => [0 4]
    (cursor :editing "axxbxx" :after "$h?xx<Enter>") => [0 1])
  (fact "`?xx` finds a previous occurrence on a previous line"
    (cursor :editing "axx\nzzy" :after "j?xx<Enter>") => [0 1]
    (cursor :editing "axxbxx\nzzy" :after "j?xx<Enter>") => [0 4])
  (fact "`?xx` shows an error when not found"
    (terminal :line :message :editing "abcyyz" :after "?xx<Enter>") => ["Did not find `xx`." :white :on :red]))
