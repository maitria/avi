(ns avi.search-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding `/`"
  (fact "`/` echoes on the command line"
    (editor :after "/") => (message-line "/"))
  (fact "characters typed after `/` echo on the commmand line"
    (editor :after "/foo") => (message-line "/foo"))
  (fact "`/` commands don't execute like `:` commands"
    (:finished? (editor :after "/q<Enter>")) => falsey)
  (fact "`/xx` jumps to the first occurrence of `xx`"
    (editor :editing "abcxxy" :after "/xx<Enter>") => (cursor [0 3]))
  (fact "`/xx` shows error when not found"
    (editor :editing "abcyyz" :after "/xx<Enter>") => (message-line ["Did not find `xx`." :white :on :red]))
  (fact "`/xx` wraps to beginning of file"
    (editor :editing "axx\nbyy" :after "j/xx<Enter>") => (cursor [0 1])
    (editor :editing "axx\nbyy" :after "j/xx<Enter>") => (message-line ["Wrapped to beginning of file!" :red]))
  (fact "`/xx` finds an occurrence on a later line"
    (editor :editing "abcyyz\nll\nfooxxy\nz" :after "/xx<Enter>") => (cursor [2 3]))
  (fact "`/xx` finds a later occurrence on the current line"
    (editor :editing "axxbxx" :after "ll/xx<Enter>") => (cursor [0 4]))
  (fact "`/` won't find the occurrence the cursor is on"
    (editor :editing "axxbxx" :after "l/xx<Enter>") => (cursor [0 4]))
  (fact "`/<Enter>` repeats the last forward search"
    (editor :editing "axxbxx" :after "/xx<Enter>/<Enter>") => (cursor [0 4])
    (editor :editing "axxbxxcxx" :after "/xx<Enter>/<Enter>/<Enter>") => (cursor [0 7]))
  (fact "empty searches are not recorded in the command history"
    (editor :editing "axxbxx" :after "/xx<Enter>/<Enter>/<C-P>") => (message-line "/xx")))

(facts "regarding `?`"
  (fact "`?` echoes on the command line"
    (editor :after "?") => (message-line "?"))
  (fact "`?xx` jumps to the previous occurrence of `xx` on the line"
    (editor :editing "axxbzz" :after "$?xx<Enter>") => (cursor [0 1])
    (editor :editing "axxbxx" :after "$?xx<Enter>") => (cursor [0 4])
    (editor :editing "axxbxx" :after "$h?xx<Enter>") => (cursor [0 1])
    (editor :editing "axxbxx" :after "$?xx<Enter>?<Enter>") => (cursor [0 1]))
  (fact "`?xx` finds a previous occurrence on a previous line"
    (editor :editing "axx\nzzy" :after "j?xx<Enter>") => (cursor [0 1])
    (editor :editing "axxbxx\nzzy" :after "j?xx<Enter>") => (cursor [0 4]))
  (fact "`?xx` wraps to end-of-file"
    (editor :editing "ayy\nbxx" :after "?xx<Enter>") => (cursor [1 1])
    (editor :editing "ayy\nbxx" :after "?xx<Enter>") => (message-line ["Wrapped to end of file!" :red]))
  (fact "`?xx` shows an error when not found"
    (editor :editing "abcyyz" :after "?xx<Enter>") => (message-line ["Did not find `xx`." :white :on :red])))

(facts "regarding `n`"
  (fact "`n` moves to the next occurrence in the same direction as the last search"
    (editor :editing "axxbxx" :after "/xx<Enter>n") => (cursor [0 4])
    (editor :editing "axxbxx\nz" :after "j?xx<Enter>n") => (cursor [0 1]))
  (fact "`n` displays the last search on the message line"
    (editor :editing "axxbxx" :after "/xx<Enter>n") => (message-line "/xx")
    (editor :editing "axxbxx\nz" :after "j?xx<Enter>n") => (message-line "?xx")))

(facts "regarding `N`"
  (fact "`N` finds the next occurrence in the opposite direction"
    (editor :editing "axxbxxcxx" :after "3l/xx<Enter>N") => (cursor [0 1])
    (editor :editing "axxbxxcxx" :after "5l?xx<Enter>N") => (cursor [0 7]))
  (fact "`N` does not change the current search direction"
    (editor :editing "axxbxxcxx" :after "3l/xx<Enter>Nn") => (cursor [0 4])
    (editor :editing "axxbxxcxx" :after "5l?xx<Enter>Nn") => (cursor [0 4])))
