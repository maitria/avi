(ns viv.core-test
  (:use midje.sweet)
  (:require [viv.core :as core]))

(defn displays
  [string _ [i j]]
  (fn [viv]
    (= string (.substring (core/screen-line viv i) j (+ j (count string))))))

(fact "it displays the content of loaded files"
  (let [viv (core/start [80 25] ["test/test.txt"])]
    viv => (displays "One" :at [0 0])
    viv => (displays "Two" :at [1 0])
    viv => (displays "Three" :at [2 0])))
