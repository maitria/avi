(ns avi.compose-test
  (:require [midje.sweet :refer :all]
            [avi.compose :refer :all]))

(fact "->* results in the outer state when given no forms"
  (->* {:foo 42} :foo) => {:foo 42})
