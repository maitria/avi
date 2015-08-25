(ns avi.command-line-mode-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding command-line mode"
  (fact "`:` echos on the command-line"
    (editor :after ":") => (message-line ":"))
  (fact "`:` places the cursor after the colon prompt"
    (editor :after ":") => (cursor [7 1]))
  (fact "characters typed after `:` echo on the command-line"
    (editor :after ":abc") => (message-line ":abc"))
  (fact "characters typed after `:` move the cursor"
    (editor :after ":a") => (cursor [7 2])
    (editor :after ":abc") => (cursor [7 4]))
  (fact "characters typed after `:` can be deleted with backspace"
    (editor :after ":abc<BS><BS>") => (cursor [7 2]))
  (fact "<BS> at position zero on the command-line cancels"
    (:mode (editor :after ":<BS>")) => :normal))
(facts "regarding `:q`"
  (fact "Avi doesn't start in the 'finished' state"
    (:finished? (editor)) => falsey)
  (fact "Typing part of `:q<Enter>` doesn't exit Avi"
    (:finished? (editor :after ":")) => falsey
    (:finished? (editor :after ":q")) => falsey)
  (fact "`:q<Enter>` exits Avi."
    (:finished? (editor :after ":q<Enter>")) => true)
  (fact "`:q!<Enter>` does not exit Avi."
    (:finished? (editor :after ":q!<Enter>")) => falsey))

(facts "regarding `:<N>`"
  (fact "`:<N><Enter>` moves to line N"
    (editor :after ":3<Enter>") => (cursor [2 0])))

(facts "regarding `:<Enter>`"
  (fact "`:<Enter>` does nothing"
    (editor :after ":<Enter>") => (cursor [0 0])))

(facts "regarding bad commands"
  (fact "`:zrbl<Enter>` doesn't change cursor position"
    (editor :after ":zrbl<Enter>") => (cursor [0 0]))
  (fact "`:blrg<Enter>` produces error message" 
    (editor :after ":blrg<Enter>") => (message-line [":blrg is not a thing" :white :on :red]))
  (fact "':foo<Enter> produces specific error message"
    (editor :after ":foo<Enter>") => (message-line [":foo is not a thing" :white :on :red]))
  (fact "error message longer than terminal width gets clipped"
    (editor :width 20 :after ":holycrapbatmanwhatdoido<Enter>") =>  (message-line [":holycrapbatmanwhatd" :white :on :red])))

(facts "regarding `:w<Enter>`"
  (fact "`:w<Enter>` writes the file"
    (file-written :editing "ABC\nDEF\nGHI" :after ":w<Enter>") => ["test.txt" "ABC\nDEF\nGHI"])
  (fact "`:w<Enter>` clears the message line (and doesn't fail)"
    (editor :editing "ABC" :after ":w<Enter>") => (message-line "")))

(facts "regarding `:wq`"
  (fact "`:wq` writes the file"
    (file-written :editing "ABC" :after ":wq<Enter>") => ["test.txt" "ABC"])
  (fact "`:wq` exits avi"
    (:finished? (editor :after ":wq<Enter>")) => true))

(facts "regarding command-line history"
  (fact "`:<C-P>` moves to previous command"
    (editor :after ":42<Enter>:<C-P>") => (message-line ":42")
    (editor :after ":42<Enter>:69<Enter>:<C-P><C-P>") => (message-line ":42"))
  (fact "`:<C-N>` moves to next command"
    (editor :after ":42<Enter>:69<Enter>:<C-P><C-P><C-N>") => (message-line ":69")))
