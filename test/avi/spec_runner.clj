(ns avi.spec-runner
  (:require [midje.sweet :refer :all])
  (:require [avi.test-helpers :refer :all]))

(defmacro facts-about
  [description & args]
  `(tabular
     (facts ~description
       (editor :editing ~'?content :after ~'?after) => (point ~'?point))
     ~@args))
