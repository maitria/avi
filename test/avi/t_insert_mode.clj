(ns avi.t-insert-mode
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding insert mode"
  (fact "`ixxx<Esc>` inserts three xs"
    (editor :editing "One" :after "ixxx<Esc>") => (line 0 "xxxOne"))
  (fact "`ixyz<Esc>` inserts `xyz`"
    (editor :editing "One\nTwo\nThree..." :after "ixyz<Esc>") => (line 0 "xyzOne"))
  (fact "`ixyz<Esc>` leaves the point on `z`"
    (editor :editing "One\nTwo\nThree..." :after "ixyz<Esc>") => (point [0 2]))

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
    (fact "`i<BS>` at beginning of line leaves the point at join"
      (editor :editing "One\nTwo\nThree..." :after "ji<BS>") => (point [0 3])))

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
    (editor :editing "One\nTwo\nThree..." :after "ix<Enter><Esc>") => (point [1 0])
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
  (fact "`oxy<Esc>` works when file has one line"
    (editor :editing "" :after "oxy<Esc>")
      => (terminal [""
                    "xy"
                    "~" :blue
                    "~" :blue
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
                    ""]))
  (facts "about Arrow Keys in Insert mode"
    (fact "<Right> moves to the right of the current position including eol cases"
      (editor :editing "One\nTwo\nThree..." :after "i<Right><Esc>") => (point [0 0])
      (editor :editing "One\nTwo\nThree..." :after "i<Right><Right><Right><Esc>") => (point [0 2])
      (editor :editing "One\nTwo\nThree..." :after "i<Right><Right><Right><Right><Right><Esc>") => (point [0 2]))
    (fact "<Left> moves to the left of the current position"
      (editor :editing "One\nTwo\nThree..." :after "i<Right><Left><Esc>") => (point [0 0])
      (editor :editing "One\nTwo\nThree..." :after "i<Right><Right><Left><Left><Left><Esc>") => (point [0 0])
      (editor :editing "One\nTwo\nThree..." :after "i<Down><Down><Right><Right><Right><Right><Right><Left><Left><Left><Left><Esc>") => (point [2 0]))
    (fact "<Down> moves to the bottom line of the current position"
      (editor :editing "One\nTwo\nThree..." :after "i<Down><Esc>") => (point [1 0])
      (editor :editing "One\nTwo\nThree..." :after "i<Right><Down><Down><Esc>") => (point [2 0])
      (editor :editing "One\nTwo\nThree..." :after "i<Down><Down><Down><Down><Right><Right><Right><Right><Esc>") => (point [2 3])
      (editor :editing "One\nTwo\nThree...\nFour" :after "i<Down><Down><Right><Right><Right><Right><Right><Right><Down><Down><Esc>") => (point [3 2]))
    (fact "<Up> moves to the top line of the current position"
      (editor :editing "One\nTwo\nThree..." :after "i<Down><Down><Right><Right><Up><Esc>") => (point [1 1])
      (editor :editing "One\nTwo\nThree..." :after "i<Down><Right><Up><Up><Esc>") => (point [0 0])
      (editor :editing "One\nTwo\nThree...\nFour\nFive" :after "<Down><Down><Down><Down><Up><Esc>") => (point [3 0])
      (editor :editing "One\nTwo\nThree..." :after "ia<Enter>b<Enter>c<Enter><Down><Down><Up><Up><Up><Esc>") => (point [2 0])
      (editor :editing "One\nTwo\nThree..." :after "ia<Enter>b<Enter>c<Enter>d<Enter>e<Enter>f<Enter><Up><Up><Up><Esc>") => (point [2 0]))))

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
  (future-fact "repeat count for `i` with `3ix<Esc>` leaves the point on last `x` added"
    (editor :editing "One" :after "3ixx<Esc>") => (point [0 2]))
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
                    ""]))
  (fact "repeat count is ignored when arrows are used during insert"
    (editor :editing "One\nTwo\nThree..." :after "3iab<Up><Up><Left>cd<Esc>")
      => (terminal ["acdbOne"
                    "Two"
                    "Three..."
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""])))

(fact "viewport is adjusted to cursor when typing in insert mode"
  (editor :editing "1\n2\n3\n4\n5\n6" :after "GA<Enter>x<Enter>y<Enter>z")
    => (terminal ["4"
                  "5"
                  "6"
                  "x"
                  "y"
                  "z"
                  "test.txt" :black :on :white
                  "--INSERT--"]))