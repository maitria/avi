(ns avi.t-spec-runner
  (:require [midje.sweet :refer :all]
            [avi.spec-runner :as sr]))

(fact "we can read the spec lines"
  (first (sr/spec-lines)) => "Normal Mode")

(fact "we can find an arrange"
  (sr/arrange) => "when editing \"hello world\"")

(facts "about the test parameters"
  (fact "it has the keystrokes"
    (:setup (sr/test-parameters)) => (contains {:after "$b"}))

  (fact "it has the resulting point"
    (:point (sr/test-parameters)) => [0 6]))
