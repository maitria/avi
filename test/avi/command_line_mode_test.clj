(ns avi.command-line-mode-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding command-line mode"
  (fact "`:` echos on the command-line"
    (terminal :line :message :after ":") => ":")
  (fact "`:` places the cursor after the colon prompt"
    (cursor :after ":") => [7 1])
  (fact "characters typed after `:` echo on the command-line"
    (terminal :line :message :after ":abc") =>":abc")
  (fact "characters typed after `:` move the cursor"
    (cursor :after ":a") => [7 2]
    (cursor :after ":abc") => [7 4])
  (fact "characters typed after `:` can be deleted with backspace"
    (cursor :after ":abc<BS><BS>") => [7 2])
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
    (cursor :after ":3<Enter>") => [2 0]))

(facts "regarding `:<Enter>`"
  (fact "`:<Enter>` does nothing"
    (cursor :after ":<Enter>") => [0 0]))

(facts "regarding bad commands"
  (fact "`:zrbl<Enter>` doesn't change cursor position"
    (cursor :after ":zrbl<Enter>") => [0 0])
  (fact "`:blrg<Enter>` produces error message" 
    (terminal :line :message :after ":blrg<Enter>") => [":blrg is not a thing" :white :on :red])
  (fact "':foo<Enter> produces specific error message"
    (terminal :line :message :after ":foo<Enter>") => [":foo is not a thing" :white :on :red])
  (fact "error message longer than terminal width gets clipped"
    (terminal :line :message :width 20 :after ":holycrapbatmanwhatdoido<Enter>") =>  [":holycrapbatmanwhatd" :white :on :red]))

(facts "regarding `:w<Enter>`"
  (fact "`:w<Enter>` writes the file"
    (file-written :editing "ABC\nDEF\nGHI" :after ":w<Enter>") => ["test.txt" "ABC\nDEF\nGHI"])
  (fact "`:w<Enter>` clears the message line (and doesn't fail)"
    (terminal :line :message :editing "ABC" :after ":w<Enter>") => ""))

(facts "regarding `:wq`"
  (fact "`:wq` writes the file"
    (file-written :editing "ABC" :after ":wq<Enter>") => ["test.txt" "ABC"])
  (fact "`:wq` exits avi"
    (:finished? (editor :after ":wq<Enter>")) => true))

(facts "regarding command-line history"
  (fact "`:<C-P>` moves to previous command"
    (terminal :line :message :after ":42<Enter>:<C-P>") => ":42"))