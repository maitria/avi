(ns avi.t-spec-runner
  (:require [midje.sweet :refer :all]
            [avi.spec-runner :as sr]))

(fact "we can read the spec lines"
  (first (sr/read-spec-lines)) => "Normal Mode")
