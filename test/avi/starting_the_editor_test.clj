(ns avi.starting-the-editor-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding starting the editor"
  (fact "When avi is passed the name of an existing file, it shows the contents and the name"
    (terminal)
      => ["One"
          "Two"
          "Three"
          "."
          "~" :blue
          "~" :blue
          "test.txt" :black :on :white
          ""])

  (fact "When avi is not passed a filename, it starts with an empty, unnamed buffer."
    (terminal :editing :nothing)
     => [""
         "~" :blue
         "~" :blue
         "~" :blue
         "~" :blue
         "~" :blue
         "[No Name]" :black :on :white
         ""])

  (fact "When avi is passed the name of a non-existent file, it starts with an empty, named buffer."
    (terminal :editing :not-found)
     => [""
         "~" :blue
         "~" :blue
         "~" :blue
         "~" :blue
         "~" :blue
         "test.txt" :black :on :white
         ""]))
