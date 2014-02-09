(ns viv.core-test
  (:use midje.sweet)
  (:require [viv.core :as core]))

(defn displays
  [string _ [i j]]
  (fn [viv]
    (= string (.substring (core/screen-line viv i) j (+ j (count string))))))

(fact "it displays the content of loaded files"
  (core/start [80 25] ["README.md"]) => (displays "# viv" :at [0 0]))
