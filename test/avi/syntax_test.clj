(ns avi.syntax-test
  (:require [midje.sweet :refer :all]
            [avi.test-helpers :refer :all]))

(facts "about clojure syntax"
  (fact "clojure parentheses are red"
    (editor :editing "(foo)") => (attributes [0 0] :red)
    (editor :editing "(foo)") => (attributes [0 4] :red)))
