(ns avi.t-splits
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "about horizontal splits"
  (fact "`:sp` splits horizontally"
    (editor :editing "One\nTwo\nThree" :after ":sp<Enter>")
      => (every-pred
           (terminal ["One"
                      "Two"
                      "test.txt                      [1,1]" :black :on :white
                      "One"
                      "Two"
                      "Three"
                      "test.txt                      [1,1]" :black :on :white
                      ""])
           (point [0 0])))
  (fact "`:sp` splits the correct pane"
    (editor :height 40 :after ":sp<Enter><C-W>j:sp<Enter>") => (point [12 0]))
  (fact "horizontal splits scroll correctly"
    (editor :editing "One\nTwo\nThree\nFour" :after ":sp<Enter>G")
      => (terminal ["Three"
                    "Four"
                    "test.txt                      [4,1]  End" :black :on :white
                    "One"
                    "Two"
                    "Three"
                    "test.txt                      [1,1]  Top" :black :on :white
                    ""]))
  (fact "horizontal splits scroll correctly to middle of buffer"
    (editor :editing "One\nTwo\nThree\nFour\nFive\nSix\nSeven\nEight" :after ":sp<Enter>G<Up><Up><Up><Up><Up>")
      => (terminal ["Three"
                    "Four"
                    "test.txt                      [3,1]  33%" :black :on :white
                    "One"
                    "Two"
                    "Three"
                    "test.txt                      [1,1]  Top" :black :on :white
                    ""]))
  (fact "two splits equalize size (with remainder to the last one)"
    (editor :height 12 :editing "One\nTwo\nThree\nFour" :after ":sp<Enter>:sp<Enter>")
      => (terminal ["One"
                    "Two" 
                    "test.txt                      [1,1]  Top" :black :on :white
                    "One"
                    "Two" 
                    "test.txt                      [1,1]  Top" :black :on :white
                    "One"
                    "Two"
                    "Three"
                    "Four"
                    "test.txt                      [1,1]  Top" :black :on :white
                    ""]))
  (fact "<C-W>j moves down a pane"
    (editor :after ":sp<Enter><C-W>j") => (point [3 0])
    (editor :height 40 :after ":sp<Enter>:sp<Enter><C-W>j") => (point [12 0])
    (editor :height 40 :after ":sp<Enter>:sp<Enter><C-W>j<C-W>j") => (point [24 0])
    (editor :after "<C-W>j") => beeped)
  (fact "<C-W>k moves up a pane"
    (editor :after ":sp<Enter><C-W>j<C-W>k") => (point [0 0])
    (editor :height 40 :after ":sp<Enter>:sp<Enter><C-W>j<C-W>j<C-W>k") => (point [12 0])
    (fact "<C-W>k always works (regression)"
      (editor :after "j:sp<Enter><C-W>j<C-W>k") => (point [1 0]))))

(facts "about vertical splits"
  (fact "`:vsp` splits vertically"
    (editor :editing "One\nTwo\nThree" :after ":vsp<Enter>")
      => (terminal ["One                |One"
                    "Two                |Two"
                    "Three              |Three"
                    "~                  |~" :blue
                    "~                  |~" :blue
                    "~                  |~" :blue
                    "test.tx  [1,1]  Top|test.txt  [1,1]  Top" :black :on :white
                    ""]))
  (fact "`<C-W>l` moves right a pane"
    (editor :editing "One\nTwo\nThree" :after ":vsp<Enter><C-W>l")
      => (point [0 20]))
  (fact "`<C-W>h` moves left a pane"
    (editor :editing "One\nTwo\nThree" :after ":vsp<Enter><C-W>l<C-W>h")
      => (point [0 0])))

(facts "about vertical and horizontal splits"
  (fact "multiple splits inside of pane area"
    (editor :width 80 :height 40 :after ":sp<Enter>:vsp<Enter>:sp<Enter>:vsp<Enter>:sp<Enter>:vsp<Enter>:sp<Enter>:vsp<Enter>")
      => did-not-beep)
  (fact "multiple splits out of pane area"
    (editor :width 20 :height 8 :after ":sp<Enter>:vsp<Enter>:sp<Enter>:vsp<Enter>:sp<Enter>:vsp<Enter>:sp<Enter>:vsp<Enter>")
      => (message-line ["No room for new Pane" :white :on :red]))
  (fact "correctly handles closing last sub-split"
    (editor :after ":vsp<Enter>:sp<Enter><C-W>l:sp<Enter>:q<Enter>:q<Enter>")
      => (terminal ["One"
                    "Two"
                    "test.txt                      [1,1]  Top" :black :on :white
                    "One"
                    "Two"
                    "Three" 
                    "test.txt                      [1,1]  Top" :black :on :white
                    ""])))
