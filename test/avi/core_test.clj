(ns avi.core-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "regarding displaying of a loaded file"
  (fact "Each line is displayed on a different line."
    (editor) => (renders "One            " :at [0 0])
    (editor) => (renders "Two            " :at [1 0])
    (editor) => (renders "Three          " :at [2 0]))
  (fact "Tildes are displayed on blank lines."
    (editor) => (renders "~              " :at [4 0] :in :blue)
    (editor) => (renders "~              " :at [7 0] :in :blue))
  (fact "The filename appears in the status bar."
    (editor) => (renders "test/test.txt  " :at [8 0] :in :black :on :white))
  (fact "The prompt line is clear."
    (editor) => (renders "               " :at [9 0])))

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
