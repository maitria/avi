(ns avi.t-spec-runner
  (:require [midje.sweet :refer :all]
            [avi.spec-runner :as sr]))

(fact "we can read the spec lines"
  (first (sr/spec-lines)) => "Normal Mode")

(fact "we can find an arrange"
  (sr/arrange) => "when editing \"hello world\"")

(fact "we can find the action"
  (sr/test-parameters) => (contains {:after "$b"}))

(fact "we can find the resulting point"
  (sr/resulting-point) => [0 6]) 
