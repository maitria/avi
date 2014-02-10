(ns viv.core-test
  (:use midje.sweet)
  (:require [viv.core :as core]))

(defn displays
  [expected & {[i j] :at,
               expected-color :in,
               expected-background :on,
               :or {expected-color :white,
                    expected-background :black}}]
  (fn [editor]
    (let [[actual-color actual-background line] (core/screen-line editor i)
          actual (.substring line j (+ j (count expected)))]
      (and
        (= expected actual)
        (= expected-color actual-color)
        (= expected-background actual-background)))))

(facts "regarding displaying of a loaded file"
  (let [editor (core/start [10 80] "test/test.txt")]
    (fact "Each line is displayed on a different line."
      editor => (displays "One" :at [0 0])
      editor => (displays "Two" :at [1 0])
      editor => (displays "Three" :at [2 0]))

    (fact "Tildes are displayed on blank lines."
      editor => (displays "~" :at [3 0] :in :blue)
      editor => (displays "~" :at [7 0] :in :blue))

    (fact "The filename appears in the status bar."
      editor => (displays "test/test.txt" :at [8 0] :in :black :on :white))))
