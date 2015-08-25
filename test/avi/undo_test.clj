(ns avi.undo-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))
(facts "about `u`"
  (fact "`xu` leaves the file the way it came"
    (editor :editing "One\nTwo\nThree..." :after "xu") => (line 0 "One"))
  (fact "`xu` leaves the cursor at 0,0"
    (editor :editing "One\nTwo\nThree..." :after "xu") => (cursor [0 0]))
  (fact "`u` tells us it's already at the oldest change"
    (editor :editing "One" :after "u") => (message-line ["Already at the oldest change" :white :on :red]))
  (fact "`xxxuu` leaves first delete"
    (editor :editing "One\nTwo\nThree..." :after "xxxuu") => (line 0 "ne"))
  (fact "`3xu` leaves file unchanged"
    (editor :editing "One\n" :after "3xu") => (line 0 "One"))
  (fact "`ddju` leaves cursor at 0,0"
    (editor :editing "One\nTwo\nThree..." :after "ddju")
      => (terminal ["One"
                    "Two"
                    "Three..."
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""])
    (editor :editing "One\nTwo\nThree..." :after "ddju") => (cursor [0 0]))
  (fact "`ddGu` adjusts viewport to cursor"
    (editor :editing ten-lines :after "ddGu") => (cursor [0 0]))
  (fact "`2ddu` undoes both lines"
    (editor :editing "One\nTwo\nThree..." :after "2ddGu")
      => (terminal ["One"
                    "Two"
                    "Three..."
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "test.txt" :black :on :white
                    ""])))

(facts "regarding undoing inserts"
  (fact "can undo an insert"
    (editor :editing "One" :after "ixyz<Esc>u") => (line 0 "One"))
  (fact "can undo repeated inserts"
    (editor :editing "One" :after "2ixy<Esc>u") => (line 0 "One")))

(facts "about <C-R>"
  (fact "`xu<C-R>` keeps the first edit"
    (editor :editing "One" :after "xu<C-R>") => (line 0 "ne"))
  (fact "`xxxuu<C-R><C-R>` keeps the first two edits"
    (editor :editing "One" :after "xxxuu<C-R><C-R>") => (line 0 ""))
  (fact "`xu<C-R>u` has no edits"
    (editor :editing "One" :after "xu<C-R>u") => (line 0 "One"))
  (fact "`<C-R>` tells us it's already at the newest change"
    (editor :editing "One" :after "<C-R>") => (message-line ["Already at the newest change" :white :on :red]))
  (fact "making a change kills the redo list"
    (editor :editing "One" :after "xuxx<C-R>") => (line 0 "e")))
