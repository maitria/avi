(ns avi.insert-mode-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding insert mode"
  (fact "`ixxx<Esc>` inserts three xs"
    (terminal :line 0 :editing "One" :after "ixxx<Esc>") => "xxxOne")
  (fact "`ixyz<Esc>` inserts `xyz`"
    (terminal :line 0 :editing "One\nTwo\nThree..." :after "ixyz<Esc>") => "xyzOne")
  (fact "`ixyz<Esc>` leaves the cursor on `z`"
    (cursor :editing "One\nTwo\nThree..." :after "ixyz<Esc>") => [0 2])

  (facts "about `<BS>` in insert mode"
    (fact "`ixy<BS>z<Esc>` inserts `xz`"
      (terminal :line 0 :editing "One\nTwo\nThree..." :after "ixy<BS>z<Esc>") => "xzOne")
    (fact "`i<BS>` at 0,0 zero beeps"
      (editor :editing "xx" :after "i<BS>") => beeped)
    (fact "`i<BS>` at 1,0 joins lines"
      (terminal :editing "One\nTwo\nThree..." :after "ji<BS><Esc>")
        => ["OneTwo"
            "Three..."
            "~" :blue
            "~" :blue
            "~" :blue
            "~" :blue
            "test.txt" :black :on :white
            ""])
    (fact "`i<BS>` at beginning of line leaves the cursor at join"
       (cursor :editing "One\nTwo\nThree..." :after "ji<BS>") => [0 3]))

  (fact "`<Esc>` in insert mode returns to normal mode"
    (:mode (editor :after "i<Esc>")) => :normal)
  (fact "Avi displays `--INSERT--` on the prompt mode when in insert mode"
    (terminal :line :message :editing "One\nTwo\nThree..." :after "i") => "--INSERT--")
  (fact "`ix<Enter>` inserts a new line"
    (terminal :editing "One\nTwo\nThree..." :after "ix<Enter><Esc>")
     => ["x"
         "One"
         "Two"
         "Three..."
         "~" :blue
         "~" :blue
         "test.txt" :black :on :white
         ""]
    (cursor :editing "One\nTwo\nThree..." :after "ix<Enter><Esc>") => [1 0]
    (terminal :editing "" :after "i<Enter><Esc>")
      => [""
          ""
          "~" :blue
          "~" :blue
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""])
  (fact "`oxy<Esc>` inserts a line below"
    (terminal :editing "One\nTwo\nThree..." :after "oxy<Esc>")
      => ["One"
          "xy"
          "Two"
          "Three..."
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""])
  (fact "`Oxy<Esc>` inserts a line here"
    (terminal :editing "One\nTwo\nThree..." :after "Oxy<Esc>")
      => ["xy"
          "One"
          "Two"
          "Three..."
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""])
  (fact "`axy<Esc>` inserts after current character"
    (terminal :editing "One\nTwo\nThree..." :after "axy<Esc>")
      => ["Oxyne"
          "Two"
          "Three..."
          "~" :blue
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""])
  (fact "`Axy<Esc>` inserts at end-of-line"
    (terminal :editing "One\nTwo\nThree..." :after "Axy<Esc>")
      => ["Onexy"
          "Two"
          "Three..."
          "~" :blue
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""]))

(facts "regarding repeating in insert mode"
  (fact "repeat count for `i` repeatedly inserts the text"
    (terminal :line 0 :editing "One" :after "3ix<Esc>") => "xxxOne"
    (terminal :line 0 :editing "One" :after "2ixy<BS><Esc>") => "xxOne"
    (terminal :editing "One" :after "3ix<Enter><Esc>")
      => ["x"
          "x"
          "x"
          "One"
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""]
  (fact "repeat count for `a` repeatedly inserts the text"
    (terminal :line 0 :editing "One" :after "3axy<Esc>") => "Oxyxyxyne")
  (fact "repeat count for `A` repeatedly inserts the text at end-of-line"
    (terminal :line 0 :editing "One" :after "3Axy<Esc>") => "Onexyxyxy"))
  (fact "repeat count for `o` makes multiple lines"
    (terminal :editing "One" :after "3oxxx<Esc>")
      => ["One"
          "xxx"
          "xxx"
          "xxx"
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""])
  (fact "repeat count for `O` makes multiple lines"
    (terminal :editing "One" :after "3Oxxx<Esc>")
      => ["xxx"
          "xxx"
          "xxx"
          "One"
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""]))

(facts "regarding undoing inserts"
  (fact "can undo an insert"
    (terminal :line 0 :editing "One" :after "ixyz<Esc>u") => "One")
  (fact "can undo repeated inserts"
    (terminal :line 0 :editing "One" :after "2ixy<Esc>u") => "One"))
