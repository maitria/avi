(ns viv.core-test
  (:use midje.sweet)
  (:require [viv.core :as core]))

(defn displays
  [expected & {[i j] :at,
               expected-color :in,
               expected-background :on,
               :or {expected-color :white,
                    expected-background :black}}]
  (fn [viv]
    (let [[actual-color actual-background line] (core/screen-line viv i)
          actual (.substring line j (+ j (count expected)))]
      (and
        (= expected actual)
        (= expected-color actual-color)
        (= expected-background actual-background)))))

(facts "regarding displaying of a loaded file:"
  (let [viv (core/start [10 80] ["test/test.txt"])]
    (fact "Each line is displayed on a different line."
      viv => (displays "One" :at [0 0])
      viv => (displays "Two" :at [1 0])
      viv => (displays "Three" :at [2 0]))

    (fact "Tildes are displayed on blank lines."
      viv => (displays "~" :at [3 0] :in :blue)
      viv => (displays "~" :at [7 0] :in :blue))

    (fact "The filename appears in the status bar."
      viv => (displays "test/test.txt" :at [8 0] :in :black :on :white))))
