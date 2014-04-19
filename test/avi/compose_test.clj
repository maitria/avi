(ns avi.compose-test
  (:require [midje.sweet :refer :all]
            [avi.compose :refer :all]))

(defn in-foo
  [state update-fn]
  (update-in state [:foo] update-fn))

(facts "about ->*"
  (->* {:foo 42} :foo) => {:foo 42}
  (->* {:foo 42} :foo inc) => {:foo 43}
  (->* {:foo 42} in-foo dec) => {:foo 41})
