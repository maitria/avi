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
  (->' 4 (+ 2) (/ 3)) => 2
  (->' 1 (if true (+ 1))) => 2
  (->' 1 (if false (+ 1))) => 1
  (->' 1 (if false (+ 1) (+ 2))) => 3
  (->' 1 (if-not true (+ 1))) => 1
  (->' 1 (if-not false (+ 1))) => 2
  (->' 1 (if-not true (+ 1) (+ 5))) => 6
  (->' 1 (if-let [x 2] (+ x))) => 3
  (->' 1 (cond true (+ 3))) => 4
  (->' 1 (cond false (+ 3) true (+ 2))) => 3
  (->' 1 (cond false (+ 3))) => 1)
