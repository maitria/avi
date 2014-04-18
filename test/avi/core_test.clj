(ns avi.core-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding displaying on-screen"
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
