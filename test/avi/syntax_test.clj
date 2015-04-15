(ns avi.syntax-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "about clojure syntax"
  (fact "clojure parentheses are red"
    (attributes :at [0 0] :editing "(foo)") => :red
    (attributes :at [0 4] :editing "(foo)") => :red))