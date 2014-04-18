(ns avi.file-handling-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding starting the editor"
  (fact "When given the name of an existing file, it shows the contents and the name"
    (editor)
     => (looks-like
          "One                 "
          "Two                 "
          "Three               "
          ".                   "
          "~                   " [:blue]
          "~                   " [:blue]
          "test.txt            " [:black :on :white]
          "                    "))

  (fact "When started without a file, it starts with an empty, unnamed buffer."
    (editor :editing :nothing)
     => (looks-like
          "                    "
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "[No Name]           " [:black :on :white]
          "                    "))

  (fact "When started with a non-existent file, it start with an empty, named buffer."
    (editor :editing :not-found)
     => (looks-like
          "                    "
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "test.txt            " [:black :on :white]
          "                    ")))
