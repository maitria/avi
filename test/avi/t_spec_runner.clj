(ns avi.t-spec-runner
  (:require [midje.sweet :refer :all]
            [avi.spec-runner :as sr]))

(facts "about the avi spec runner"
  (fact "we get the lines from the file"
    (first (sr/spec-lines)) => "Normal Mode")

  (fact "we can find an arrange"
    (sr/arrange) => "when editing \"hello world\"")

  (facts "about the test parameters"
    (fact "it has the keystrokes"
      (:setup (sr/test-parameters)) => (contains {:after "$b"}))
    (fact "it has the resulting point"
      (:point (sr/test-parameters)) => [0 6])))

