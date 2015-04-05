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
  (fact "`/xx` wraps to beginning of file"
    (cursor :editing "axx\nbyy" :after "j/xx<Enter>") => [0 1]
    (terminal :line :message :editing "axx\nbyy" :after "j/xx<Enter>") => ["Wrapped to beginning of file!" :red])
  (fact "`/xx` finds an occurrence on a later line"
    (cursor :editing "abcyyz\nll\nfooxxy\nz" :after "/xx<Enter>") => [2 3])
  (fact "`/xx` finds a later occurrence on the current line"
    (cursor :editing "axxbxx" :after "ll/xx<Enter>") => [0 4])
  (fact "`/` won't find the occurrence the cursor is on"
    (cursor :editing "axxbxx" :after "l/xx<Enter>") => [0 4])
  (fact "`/<Enter>` repeats the last forward search"
    (cursor :editing "axxbxx" :after "/xx<Enter>/<Enter>") => [0 4]
    (cursor :editing "axxbxxcxx" :after "/xx<Enter>/<Enter>/<Enter>") => [0 7])
  (fact "empty searches are not recorded in the command history"
    (terminal :line :message :editing "axxbxx" :after "/xx<Enter>/<Enter>/<C-P>") => "/xx"))

(facts "regarding `?`"
  (fact "`?` echoes on the command line"
    (terminal :line :message :after "?") => "?")
  (fact "`?xx` jumps to the previous occurrence of `xx` on the line"
    (cursor :editing "axxbzz" :after "$?xx<Enter>") => [0 1]
    (cursor :editing "axxbxx" :after "$?xx<Enter>") => [0 4]
    (cursor :editing "axxbxx" :after "$h?xx<Enter>") => [0 1]
    (cursor :editing "axxbxx" :after "$?xx<Enter>?<Enter>") => [0 1])
  (fact "`?xx` finds a previous occurrence on a previous line"
    (cursor :editing "axx\nzzy" :after "j?xx<Enter>") => [0 1]
    (cursor :editing "axxbxx\nzzy" :after "j?xx<Enter>") => [0 4])
  (fact "`?xx` wraps to end-of-file"
    (cursor :editing "ayy\nbxx" :after "?xx<Enter>") => [1 1]
    (terminal :line :message :editing "ayy\nbxx" :after "?xx<Enter>") => ["Wrapped to end of file!" :red])
  (fact "`?xx` shows an error when not found"
    (terminal :line :message :editing "abcyyz" :after "?xx<Enter>") => ["Did not find `xx`." :white :on :red]))

(facts "regarding `n`"
  (fact "`n` moves to the next occurrence in the same direction as the last search"
    (cursor :editing "axxbxx" :after "/xx<Enter>n") => [0 4]
    (cursor :editing "axxbxx\nz" :after "j?xx<Enter>n") => [0 1])
  (fact "`n` displays the last search on the message line"
    (terminal :line :message :editing "axxbxx" :after "/xx<Enter>n") => "/xx"
    (terminal :line :message :editing "axxbxx\nz" :after "j?xx<Enter>n") => "?xx"))

(facts "regarding `N`"
  (fact "`N` finds the next occurrence in the opposite direction"
    (cursor :editing "axxbxxcxx" :after "3l/xx<Enter>N") => [0 1]
    (cursor :editing "axxbxxcxx" :after "5l?xx<Enter>N") => [0 7])
  (fact "`N` does not change the current search direction"
    (cursor :editing "axxbxxcxx" :after "3l/xx<Enter>Nn") => [0 4]
    (cursor :editing "axxbxxcxx" :after "5l?xx<Enter>Nn") => [0 4]))
