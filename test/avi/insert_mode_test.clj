(ns avi.insert-mode-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding insert mode"
  (fact "`ixxx<Esc>` inserts three xs"
    (editor :editing "One" :after "ixxx<Esc>") => (line 0 "xxxOne"))
  (fact "`ixyz<Esc>` inserts `xyz`"
    (editor :editing "One\nTwo\nThree..." :after "ixyz<Esc>") => (line 0 "xyzOne"))
  (fact "`ixyz<Esc>` leaves the cursor on `z`"
    (editor :editing "One\nTwo\nThree..." :after "ixyz<Esc>") => (cursor [0 2]))

  (facts "about `<BS>` in insert mode"
    (fact "`ixy<BS>z<Esc>` inserts `xz`"
      (editor :editing "One\nTwo\nThree..." :after "ixy<BS>z<Esc>") => (line 0 "xzOne"))
    (fact "`i<BS>` at 0,0 zero beeps"
      (editor :editing "xx" :after "i<BS>") => beeped)
    (fact "`i<BS>` at 1,0 joins lines"
      (editor :editing "One\nTwo\nThree..." :after "ji<BS><Esc>")
        => (terminal ["OneTwo"
                      "Three..."
                      "~" :blue
                      "~" :blue
                      "~" :blue
                      "~" :blue
                      "test.txt" :black :on :white
                      ""]))
    (fact "`i<BS>` at beginning of line leaves the cursor at join"
      (editor :editing "One\nTwo\nThree..." :after "ji<BS>") => (cursor [0 3])))

  (fact "`<Esc>` in insert mode returns to normal mode"
    (editor :after "i<Esc>") => (mode :normal))
  (fact "Avi displays `--INSERT--` on the prompt mode when in insert mode"
    (editor :editing "One\nTwo\nThree..." :after "i") => (message-line "--INSERT--"))
  (fact "`ix<Enter>` inserts a new line"
    (editor :editing "One\nTwo\nThree..." :after "ix<Enter><Esc>")
     => (terminal ["x"
                   "One"
                   "Two"
                   "Three..."
                   "~" :blue
                   "~" :blue
                   "test.txt" :black :on :white
                   ""])
    (editor :editing "One\nTwo\nThree..." :after "ix<Enter><Esc>") => (cursor [1 0])
    (editor :editing "" :after "i<Enter><Esc>")
      => (terminal [""
                    ""
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""]))
  (fact "`oxy<Esc>` inserts a line below"
    (editor :editing "One\nTwo\nThree..." :after "oxy<Esc>")
      => (terminal ["One"
                    "xy"
                    "Two"
                    "Three..."
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""]))
  (fact "`Oxy<Esc>` inserts a line here"
    (editor :editing "One\nTwo\nThree..." :after "llOxy<Esc>")
      => (terminal ["xy"
                    "One"
                    "Two"
                    "Three..."
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""]))
  (fact "`axy<Esc>` inserts after current character"
    (editor :editing "One\nTwo\nThree..." :after "axy<Esc>") => (line 0 "Oxyne"))
  (fact "`a` behaves like `i` on a zero-length line"
    (editor :editing "" :after "axy<Esc>") => (line 0 "xy"))
  (fact "`Axy<Esc>` inserts at end-of-line"
    (editor :editing "One\nTwo\nThree..." :after "Axy<Esc>")
      => (terminal ["Onexy"
                    "Two"
                    "Three..."
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""])))

(facts "regarding repeating in insert mode"
  (fact "repeat count for `i` repeatedly inserts the text"
    (editor :editing "One" :after "3ix<Esc>") => (line 0 "xxxOne")
    (editor :editing "One" :after "2ixy<BS><Esc>") => (line 0 "xxOne")
    (editor :editing "One" :after "3ix<Enter><Esc>")
      => (terminal ["x"
                    "x"
                    "x"
                    "One"
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""])
  (fact "repeat count for `a` repeatedly inserts the text"
    (editor :editing "One" :after "3axy<Esc>") => (line 0 "Oxyxyxyne"))
  (fact "repeat count for `A` repeatedly inserts the text at end-of-line"
    (editor :editing "One" :after "3Axy<Esc>") => (line 0 "Onexyxyxy")))
  (fact "repeat count for `o` makes multiple lines"
    (editor :editing "One" :after "3oxxx<Esc>")
      => (terminal ["One"
                    "xxx"
                    "xxx"
                    "xxx"
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""]))
  (fact "repeat count for `O` makes multiple lines"
    (editor :editing "One" :after "3Oxxx<Esc>")
      => (terminal ["xxx"
                    "xxx"
                    "xxx"
                    "One"
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""])))
