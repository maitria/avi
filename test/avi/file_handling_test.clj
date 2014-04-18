(ns avi.file-handling-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding being started with a file"
  (fact "It starts with a named buffer with the file's contents."
    (editor)
     => (looks-like
          "One                 "
          "Two                 "
          "Three               "
          ".                   "
          "~                   " [:blue]
          "~                   " [:blue]
          "test.txt            " [:black :on :white]
          "                    ")))

(facts "regarding being started with no file"
  (fact "It starts with an empty, unnamed buffer."
    (editor :editing :nothing)
     => (looks-like
          "                    "
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "~                   " [:blue]
          "[No Name]           " [:black :on :white]
          "                    ")))

(facts "regarding being started with a non-existant file"
  (fact "It starts with an empty, named buffer."
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
