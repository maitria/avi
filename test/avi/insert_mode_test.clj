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
    (editor :editing "One\nTwo\nThree..." :after "ixyz<Esc>")
     => (looks-like
          "xyzOne              "
          "Two                 "
          "Three...            "
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "test.txt            " [:black :on :white]
          "                    "))
  (fact "`ixy<BS>z<Esc>` inserts `xz`"
    (editor :editing "One\nTwo\nThree..." :after "ixy<BS>z<Esc>")
     => (looks-like
          "xzOne               "
          "Two                 "
          "Three...            "
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "test.txt            " [:black :on :white]
          "                    "))
  (fact "`<Esc>` in insert mode returns to normal mode"
    (:mode (editor :after "i<Esc>")) => :normal)
  (fact "Avi displays `--INSERT--` on the prompt mode when in insert mode"
    (editor :editing "One\nTwo\nThree..." :after "i")
     => (looks-like
          "One                 "
          "Two                 "
          "Three...            "
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "test.txt            " [:black :on :white]
          "--INSERT--          "))
  (fact "`i<Enter>` inserts a new line"
    (editor :editing "One\nTwo\nThree..." :after "ix<Enter><Esc>")
     => (looks-like
          "x                   "
          "One                 "
          "Two                 "
          "Three...            "
          "~                   " [:blue]
          "~                   " [:blue]
          "test.txt            " [:black :on :white]
          "                    ")
    (cursor :editing "One\nTwo\nThree..." :after "ix<Enter><Esc>") => [1 0]))
