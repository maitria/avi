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
    (fact "`i<BS>` at 0,0 zero beeps"
      (editor :editing "xx" :after "i<BS>") => beeped)
    (fact "`i<BS>` at 1,0 joins lines"
      (terminal :editing "One\nTwo\nThree..." :after "ji<BS><Esc>")
        => ["OneTwo              "
            "Three...            "
            "~                   " :blue
            "~                   " :blue
            "~                   " :blue
            "~                   " :blue
            "test.txt            " :black :on :white
            "                    "])
    (fact "`i<BS>` at beginning of line leaves the cursor at join"
       (cursor :editing "One\nTwo\nThree..." :after "ji<BS>") => [0 3]))

  (fact "`<Esc>` in insert mode returns to normal mode"
    (:mode (editor :after "i<Esc>")) => :normal)
  (fact "Avi displays `--INSERT--` on the prompt mode when in insert mode"
    (status-line :editing "One\nTwo\nThree..." :after "i") => "--INSERT--          ")
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
    (cursor :editing "One\nTwo\nThree..." :after "ix<Enter><Esc>") => [1 0]
    (terminal :editing "" :after "i<Enter><Esc>")
      => ["                    "
          "                    "
          "~                   " :blue
          "~                   " :blue
          "~                   " :blue
          "~                   " :blue
          "test.txt            " :black :on :white
          "                    "])
  (fact "`oxy<Esc>` inserts a line below"
    (terminal :editing "One\nTwo\nThree..." :after "oxy<Esc>")
      => ["One                 "
          "xy                  "
          "Two                 "
          "Three...            "
          "~                   " :blue
          "~                   " :blue
          "test.txt            " :black :on :white
          "                    "])
  (fact "`Oxy<Esc>` inserts a line here"
    (terminal :editing "One\nTwo\nThree..." :after "Oxy<Esc>")
      => ["xy                  "
          "One                 "
          "Two                 "
          "Three...            "
          "~                   " :blue
          "~                   " :blue
          "test.txt            " :black :on :white
          "                    "]))
