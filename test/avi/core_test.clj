(ns avi.core-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(def ten-lines
  (str "One\nTwo\nThree\nFour\nFive\nSix\n"
       "Seven\nEight\nNine\nTen"))

(facts "regarding displaying of a loaded file"
  (editor)
   => (looks-like
        "One            "
        "Two            "
        "Three          "
        ".              "
        "~              " [:blue]
        "~              " [:blue]
        "test/test.txt  " [:black :on :white]
        "               ")
  (editor :when-editing ten-lines)
   => (looks-like
        "One            "
        "Two            "
        "Three          "
        "Four           "
        "Five           "
        "Six            "
        "test/test.txt  " [:black :on :white]
        "               "))

(facts "regarding scrolling"
  (fact "line-wise cursor movement will keep the cursor in the viewport"
    (editor :when-editing ten-lines :after-typing "6j")
     => (looks-like
          "Two            "
          "Three          "
          "Four           "
          "Five           "
          "Six            "
          "Seven          "
          "test/test.txt  " [:black :on :white]
          "               ")))

(facts "regarding quitting"
  (fact "It doesn't start in the 'finished' state."
    (:mode (editor)) =not=> :finished
    (:mode (editor :after-typing ":")) =not=> :finished
    (:mode (editor :after-typing ":q")) =not=> :finished)
  (fact "It exits after `:q<CR>`."
    (:mode (editor :after-typing ":q\r")) => :finished))

(facts "regarding screen resizes"
  (fact "It updates the editor columns."
    (:columns (editor :after-receiving [:resize [10 50]])) => 50
  (fact "It updates the editor lines."
    (:lines (editor :after-receiving [:resize [30 80]])) => 30)))
