(ns avi.t-starting-the-editor
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding starting the editor"
  (fact "When avi is passed the name of an existing file, it shows the contents and the name"
    (editor)
      => (terminal ["One"
                    "Two"
                    "Three"
                    "."
                    "~" :blue
                    "~" :blue
                    "test.txt   [1,1]" :black :on :white
                    ""]))

  (fact "When avi is not passed a filename, it starts with an empty, unnamed document"
    (editor :editing :nothing)
      => (terminal [""
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "[No Name]   [1,1]" :black :on :white
                    ""]))

  (fact "When avi is passed the name of a non-existent file, it starts with an empty, named document"
    (editor :editing :not-found)
      => (terminal [""
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "~" :blue
                    "test.txt   [1,1]" :black :on :white
                    ""])))
