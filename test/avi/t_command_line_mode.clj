(ns avi.t-command-line-mode
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding command-line mode"
  (fact "`:` echos on the command-line"
    (editor :after ":") => (message-line ":"))
  (fact "`:` places the point after the colon prompt"
    (editor :after ":") => (point [7 1]))
  (fact "characters typed after `:` echo on the command-line"
    (editor :after ":abc") => (message-line ":abc"))
  (fact "characters typed after `:` move the point"
    (editor :after ":a") => (point [7 2])
    (editor :after ":abc") => (point [7 4]))
  (fact "characters typed after `:` can be deleted with backspace"
    (editor :after ":abc<BS><BS>") => (point [7 2]))
  (fact "<BS> at position zero on the command-line cancels"
    (editor :after ":<BS>") => (mode :normal)
    (editor :after ":<BS>") => (message-line ""))
  (fact "`<Esc>` cancels command-line mode"
    (editor :after ":xx<Esc>") => (mode :normal)
    (editor :after ":xx<Esc>") => (message-line "")))

(facts "regarding `:q`"
  (fact "Avi doesn't start in the 'finished' state"
    (editor) => unfinished?)
  (fact "Typing part of `:q<Enter>` doesn't exit Avi"
    (editor :after ":") => unfinished?
    (editor :after ":q") => unfinished?)
  (fact "`:q<Enter>` exits Avi"
    (editor :after ":q<Enter>") => finished?)
  (fact "`:q!<Enter>` does not exit Avi"
    (editor :after ":q!<Enter>") => unfinished?)
  (fact "`:sp<Enter>:q<Enter>` does not exit Avi"
    (editor :after ":sp<Enter>:q<Enter>") => unfinished?)
  (fact "`:sp<Enter>:q<Enter>:q<Enter>` exits Avi"
    (editor :after ":sp<Enter>:q<Enter>:q<Enter>") => finished?))

(facts "regarding `:<N>`"
  (fact "`:<N><Enter>` moves to line N"
    (editor :after ":3<Enter>") => (point [2 0])))

(facts "regarding `:<Enter>`"
  (fact "`:<Enter>` does nothing"
    (editor :after ":<Enter>") => (point [0 0])))

(facts "regarding bad commands"
  (fact "`:zrbl<Enter>` doesn't change point position"
    (editor :after ":zrbl<Enter>") => (point [0 0]))
  (fact "`:blrg<Enter>` produces error message" 
    (editor :after ":blrg<Enter>") => (message-line [":blrg is not a thing" :white :on :red]))
  (fact "':foo<Enter> produces specific error message"
    (editor :after ":foo<Enter>") => (message-line [":foo is not a thing" :white :on :red]))
  (fact "error message longer than terminal width gets clipped"
    (editor :width 20 :after ":holycrapbatmanwhatdoido<Enter>") =>  (message-line [":holycrapbatmanwhatd" :white :on :red])))

(facts "regarding `:w<Enter>`"
  (fact "`:w<Enter>` writes the file"
    (editor :editing "ABC\nDEF\nGHI" :after ":w<Enter>") => (wrote-file "test.txt" "ABC\nDEF\nGHI"))
  (fact "`:w<Enter>` clears the message line (and doesn't fail)"
    (editor :editing "ABC" :after ":w<Enter>") => (message-line "")))

(facts "regarding `:wq`"
  (fact "`:wq` writes the file"
    (editor :editing "ABC" :after ":wq<Enter>") => (wrote-file "test.txt" "ABC"))
  (fact "`:wq` exits avi"
    (editor :after ":wq<Enter>") => finished?))

(facts "regarding command-line history"
  (fact "`:<C-P>` moves to previous command"
    (editor :after ":42<Enter>:<C-P>") => (message-line ":42")
    (editor :after ":42<Enter>:69<Enter>:<C-P><C-P>") => (message-line ":42"))
  (fact "`:<C-N>` moves to next command"
    (editor :after ":42<Enter>:69<Enter>:<C-P><C-P><C-N>") => (message-line ":69")))
