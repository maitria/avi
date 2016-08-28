(ns avi.t-splits
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "about horizontal splits"
  (fact "`:sp` splits horizontally"
    (editor :editing "One\nTwo\nThree" :after ":sp<Enter>")
      => (every-pred
           (terminal ["One"
                      "Two"
                      "test.txt" :black :on :white
                      "One"
                      "Two"
                      "Three"
                      "test.txt" :black :on :white
                      ""])
           (point [0 0])))
  (fact "horizontal splits scroll correctly"
    (editor :editing "One\nTwo\nThree\nFour" :after ":sp<Enter>G")
      => (terminal ["Three"
                    "Four"
                    "test.txt" :black :on :white
                    "One"
                    "Two"
                    "Three"
                    "test.txt" :black :on :white
                    ""]))
  (fact "two splits equalize size (with remainder to the last one)"
    (editor :editing "One\nTwo\nThree\nFour" :after ":sp<Enter>:sp<Enter>")
      => (terminal ["One"
                    "test.txt" :black :on :white
                    "One"
                    "test.txt" :black :on :white
                    "One"
                    "Two"
                    "test.txt" :black :on :white
                    ""]))
  (fact "<C-W>j moves down a pane"
    (editor :after ":sp<Enter><C-W>j") => (point [3 0])
    (editor :after ":sp<Enter>:sp<Enter><C-W>j") => (point [2 0])
    (editor :after ":sp<Enter>:sp<Enter><C-W>j<C-W>j") => (point [4 0])
    (editor :after "<C-W>j") => beeped)
  (fact "<C-W>k moves up a pane"
    (editor :after ":sp<Enter><C-W>j<C-W>k") => (point [0 0])
    (editor :after ":sp<Enter>:sp<Enter><C-W>j<C-W>j<C-W>k") => (point [2 0])
    (fact "<C-W>k always works (regression)"
      (editor :after "j:sp<Enter><C-W>j<C-W>k") => (point [1 0]))))
