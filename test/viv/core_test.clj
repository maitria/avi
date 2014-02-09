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

(facts "about loading a file on start"
  (let [viv (core/start [10 80] ["test/test.txt"])]
    (fact "it displays the content of loaded files"
      viv => (displays "One" :at [0 0])
      viv => (displays "Two" :at [1 0])
      viv => (displays "Three" :at [2 0]))

    (fact "it displays tildes after the file contents"
      viv => (displays "~" :at [3 0] :in :blue)
      viv => (displays "~" :at [7 0] :in :blue))

    (fact "it displays the filename in the status bar"
      viv => (displays "test/test.txt" :at [8 0] :in :black :on :white))))
