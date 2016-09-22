(ns avi.t-core
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding displaying in the terminal"
  (fact "It can display content longer than will fit in document viewport."
    (editor :editing ten-lines)
       => (terminal-buffer ["One"
                     "Two"
                     "Three"
                     "Four"
                     "Five"
                     "Six"])))

(facts "regarding resizing of terminal window"
  (fact "When the terminal window is resized, it updates the editor viewport size."
    (editor :after "<Resize [17 42]>") => (viewport-size [17 42]))
  (fact "When the terminal window is resized, it updates the pane size."
    (editor :editing ten-lines :after "<Resize [12 20]>G")
     => (terminal-buffer ["One"
                   "Two"
                   "Three"
                   "Four"
                   "Five"
                   "Six"
                   "Seven"
                   "Eight"
                   "Nine"
                   "Ten"]))
  (fact "When the terminal window is resized, the point stays inside the lens."
    (editor :editing ten-lines :after "G<Resize [5 20]>") => (point [2 0])))
