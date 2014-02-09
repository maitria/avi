(ns viv.core-test
  (:use midje.sweet)
  (:require [viv.core :as core]))

(defn displays
  [string _ [i j]]
  (fn [viv]
    (= string (.substring (core/screen-line viv i) j (+ j (count string))))))

(fact "it displays the content of loaded files"
  (let [viv (core/start [80 25] ["README.md"])]
    viv => (displays "# viv" :at [0 0])
    viv => (displays "## Usage" :at [4 0])))
