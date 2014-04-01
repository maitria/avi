(ns avi.command-line-mode-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding command-line mode"
  (fact "`:` echos on the command-line"
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
  (fact "`:` places the cursor after the colon prompt"
    (cursor :after ":") => [7 1])
  (fact "characters typed after `:` echo on the command-line"
    (editor :after ":abc")
     => (looks-like
          "One                 "
          "Two                 "
          "Three               "
          ".                   "
          "~                   " [:blue]
          "~                   " [:blue]
          "test.txt            " [:black :on :white]
          ":abc                ")))
