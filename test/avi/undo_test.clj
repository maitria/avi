(ns avi.undo-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))
(facts "about `u`"
  (fact "`xu` leaves the file the way it came"
    (terminal :line 0 :editing "One\nTwo\nThree..." :after "xu") => "One")
  (fact "`xu` leaves the cursor at 0,0"
    (cursor :editing "One\nTwo\nThree..." :after "xu") => [0 0])
  (fact "`u` tells us it's already at the oldest change"
    (terminal :line :message :editing "One" :after "u") => ["Already at the oldest change" :white :on :red])
  (fact "`xxxuu` leaves first delete"
    (terminal :line 0 :editing "One\nTwo\nThree..." :after "xxxuu") => "ne")
  (fact "`ddju` leaves cursor at 0,0"
    (terminal :editing "One\nTwo\nThree..." :after "ddju")
      => ["One"
          "Two"
          "Three..."
          "~" :blue
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""]
    (cursor :editing "One\nTwo\nThree..." :after "ddju") => [0 0])
  (fact "`ddGu` adjusts viewport to cursor"
    (cursor :editing ten-lines :after "ddGu") => [0 0])
  (fact "`2ddu` undoes both lines"
    (terminal :editing "One\nTwo\nThree..." :after "2ddGu") =>
        ["One"
         "Two"
         "Three..."
         "~" :blue
         "~" :blue
         "~" :blue
         "test.txt" :black :on :white
         ""]))

(facts "regarding undoing inserts"
  (fact "can undo an insert"
    (terminal :line 0 :editing "One" :after "ixyz<Esc>u") => "One")
  (fact "can undo repeated inserts"
    (terminal :line 0 :editing "One" :after "2ixy<Esc>u") => "One"))

(facts "about <C-R>"
  (fact "`xu<C-R>` keeps the first edit"
    (terminal :line 0 :editing "One" :after "xu<C-R>") => "ne")
  (fact "`xxxuu<C-R><C-R>` keeps the first two edits"
    (terminal :line 0 :editing "One" :after "xxxuu<C-R><C-R>") => "")
  (fact "`xu<C-R>u` has no edits"
    (terminal :line 0 :editing "One" :after "xu<C-R>u") => "One")
  (fact "`<C-R>` tells us it's already at the newest change"
    (terminal :line :message :editing "One" :after "<C-R>") => ["Already at the newest change" :white :on :red])
  (fact "making a change kills the redo list"
    (terminal :line 0 :editing "One" :after "xuxx<C-R>") => "e"))
