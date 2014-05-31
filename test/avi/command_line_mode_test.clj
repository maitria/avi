(ns avi.command-line-mode-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding command-line mode"
  (fact "`:` echos on the command-line"
    (terminal :after ":")
      => ["One                 "
          "Two                 "
          "Three               "
          ".                   "
          "~                   " :blue
          "~                   " :blue
          "test.txt            " :black :on :white
          ":                   "])
  (fact "`:` places the cursor after the colon prompt"
    (cursor :after ":") => [7 1])
  (fact "characters typed after `:` echo on the command-line"
    (terminal :after ":abc")
     => ["One                 "
         "Two                 "
         "Three               "
         ".                   "
         "~                   " :blue
         "~                   " :blue
         "test.txt            " :black :on :white
         ":abc                "])
  (fact "characters typed after `:` move the cursor"
    (cursor :after ":a") => [7 2]
    (cursor :after ":abc") => [7 4])
  (fact "characters typed after `:` can be deleted with backspace"
    (cursor :after ":abc<BS><BS>") => [7 2])
  (fact "<BS> at position zero on the command-line cancels"
    (:mode (editor :after ":<BS>")) => :normal))
(facts "regarding `:q`"
  (fact "Avi doesn't start in the 'finished' state"
    (:mode (editor)) =not=> :finished)
  (fact "Typing part of `:q<Enter>` doesn't exit Avi"
    (:mode (editor :after ":")) =not=> :finished
    (:mode (editor :after ":q")) =not=> :finished)
  (fact "`:q<Enter>` exits Avi."
    (:mode (editor :after ":q<Enter>")) => :finished)
  (fact "`:q!<Enter>` does not exit Avi."
    (:mode (editor :after ":q!<Enter>")) =not=> :finished))

(facts "regarding `:<N>`"
  (fact "`:<N><Enter>` moves to line N"
    (cursor :after ":3<Enter>") => [2 0]))

(facts "regarding `:<Enter>`"
  (fact "`:<Enter>` does nothing"
    (cursor :after ":<Enter>") => [0 0]))

(facts "regarding bad commands"
  (fact "`:badcommand<Enter>` doesn't change cursor position"
    (cursor :after ":badcommand<Enter>") => [0 0]))

(facts "regarding `:w<Enter>`"
  (fact "`:w<Enter>` writes the file"
    (file-written :editing "ABC\nDEF\nGHI" :after ":w<Enter>") => ["test.txt" "ABC\nDEF\nGHI"]))

(facts "regarding `:wq`"
  (fact "`:wq` writes the file"
    (file-written :editing "ABC" :after ":wq<Enter>") => ["test.txt" "ABC"])
  (fact "`:wq` exits avi"
    (:mode (editor :after ":wq<Enter>")) => :finished))
