(ns avi.core-test
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
          "                    "))
  (fact "It can display files longer than the screen."
    (editor :editing ten-lines)
     => (looks-like
          "One                 "
          "Two                 "
          "Three               "
          "Four                "
          "Five                "
          "Six                 "
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

(facts "regarding quitting"
  (fact "It doesn't start in the 'finished' state."
    (:mode (editor)) =not=> :finished
    (:mode (editor :after ":")) =not=> :finished
    (:mode (editor :after ":q")) =not=> :finished)
  (fact "It exits after `:q<Enter>`."
    (:mode (editor :after ":q<Enter>")) => :finished))

(facts "regarding screen resizes"
  (fact "It updates the editor size."
    (:size (editor :after "<Resize [17 42]>")) => [17 42])
  (fact "It updates the buffer's size."
    (editor :editing ten-lines :after "<Resize [12 20]>G")
     => (looks-like
          "One                 "
          "Two                 "
          "Three               "
          "Four                "
          "Five                "
          "Six                 "
          "Seven               "
          "Eight               "
          "Nine                "
          "Ten                 "
          "test.txt            " [:black :on :white]
          "                    "))
  (fact "It adjusts the viewport for the cursor."
    (cursor :editing ten-lines :after "G<Resize [5 20]>") => [2 0]))
