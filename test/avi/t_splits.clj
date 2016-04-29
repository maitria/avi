(ns avi.t-splits
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "about horizontal splits"
  (fact "`:sp` splits horizontally"
    (editor :editing "One\nTwo\nThree" :after ":sp<Enter>")
      => (terminal ["One"
                    "Two"
                    "test.txt" :black :on :white
                    "One"
                    "Two"
                    "Three"
                    "test.txt" :black :on :white
                    ""]))
  (fact "horizontal splits scroll correctly"
    (editor :editing "One\nTwo\nThree\nFour" :after ":sp<Enter>G")
      => (terminal ["Three"
                    "Four"
                    "test.txt" :black :on :white
                    "One"
                    "Two"
                    "Three"
                    "test.txt" :black :on :white
                    ""])))
