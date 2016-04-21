(ns avi.t-splits
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "about horizontal splits"
  (editor :editing "One\nTwo\nThree" :after ":sp<Enter>")
    => (terminal ["One"
                  "Two"
                  "test.txt" :black :on :white
                  "One"
                  "Two"
                  "Three"
                  "test.txt" :black :on :white
                  ""]))
