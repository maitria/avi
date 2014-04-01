(ns avi.command-line-mode-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding command-line mode"
  (editor :after ":")
   => (looks-like
        "One                 "
        "Two                 "
        "Three               "
        ".                   "
        "~                   " [:blue]
        "~                   " [:blue]
        "test.txt            " [:black :on :white]
        ":                   "))
