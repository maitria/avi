(ns avi.compose-test
  (:require [midje.sweet :refer :all]
            [avi.compose :refer :all]))

(facts "about ->*"
  (->* {:foo 42} :foo) => {:foo 42}
  (->* {:foo 42} :foo inc) => {:foo 43})
