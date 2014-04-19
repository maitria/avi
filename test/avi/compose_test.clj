(ns avi.compose-test
  (:require [midje.sweet :refer :all]
            [avi.compose :refer :all]))

(defn foo
  [state update-fn]
  (update-in state [:foo] update-fn))

(facts "about in->"
  (in-> {:foo 42} :foo) => {:foo 42}
  (in-> {:foo 42} :foo inc) => {:foo 43}
  (in-> {:foo 42} foo dec) => {:foo 41})

(facts "about ->'"
  (->' 4 (+ 2) (/ 3)) => 2)
