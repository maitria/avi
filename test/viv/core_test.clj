(ns viv.core-test
  (:use midje.sweet)
  (:require [viv.core :as core]))

(defn displays
  [expected _ [i j]]
  (fn [viv]
    (let [line (core/screen-line viv i)
          actual (.substring line j (+ j (count expected)))]
      (= expected actual))))

(fact "it displays the content of loaded files"
  (let [viv (core/start [80 25] ["test/test.txt"])]
    viv => (displays "One" :at [0 0])
    viv => (displays "Two" :at [1 0])
    viv => (displays "Three" :at [2 0])))
