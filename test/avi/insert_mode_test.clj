(ns avi.insert-mode-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding insert mode"
  (fact "`ixxx<Esc>` inserts three xs"
    (terminal :editing "One\nTwo\nThree..." :after "ixxx<Esc>")
     => ["xxxOne              "
         "Two                 "
         "Three...            "
         "~                   " :blue
         "~                   " :blue
         "~                   " :blue
         "test.txt            " :black :on :white
         "                    "])
  (fact "`ixyz<Esc>` inserts `xyz`"
    (terminal :editing "One\nTwo\nThree..." :after "ixyz<Esc>")
     => ["xyzOne              "
         "Two                 "
         "Three...            "
         "~                   " :blue
         "~                   " :blue
         "~                   " :blue
         "test.txt            " :black :on :white
         "                    "])
  (fact "`ixyz<Esc>` leaves the cursor on `z`"
    (cursor :editing "One\nTwo\nThree..." :after "ixyz<Esc>") => [0 2])

  (facts "about `<BS>` in insert mode"
    (fact "`ixy<BS>z<Esc>` inserts `xz`"
      (terminal :editing "One\nTwo\nThree..." :after "ixy<BS>z<Esc>")
          => ["xzOne               "
              "Two                 "
              "Three...            "
              "~                   " :blue
              "~                   " :blue
              "~                   " :blue
              "test.txt            " :black :on :white
              "                    "])
    (fact "`i<BS>` on line zero beeps"
      (editor :editing "xx" :after "i<BS>") => beeped))

  (fact "`<Esc>` in insert mode returns to normal mode"
    (:mode (editor :after "i<Esc>")) => :normal)
  (fact "Avi displays `--INSERT--` on the prompt mode when in insert mode"
    (terminal :editing "One\nTwo\nThree..." :after "i")
     => ["One                 "
         "Two                 "
         "Three...            "
         "~                   " :blue
         "~                   " :blue
         "~                   " :blue
         "test.txt            " :black :on :white
         "--INSERT--          "])
  (fact "`ix<Enter>` inserts a new line"
    (terminal :editing "One\nTwo\nThree..." :after "ix<Enter><Esc>")
     => ["x                   "
         "One                 "
         "Two                 "
         "Three...            "
         "~                   " :blue
         "~                   " :blue
         "test.txt            " :black :on :white
         "                    "]
    (cursor :editing "One\nTwo\nThree..." :after "ix<Enter><Esc>") => [1 0]))
