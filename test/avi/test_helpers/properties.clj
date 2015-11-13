(ns avi.test-helpers.properties
  "A method for using test.check properties in midje"
  (:require [clojure.test.check :as tc]
            [midje.sweet :refer :all]))

(defn holds
  [result]
  (= true (:result result)))

(defmacro property
  ([descr prop]
   `(property ~descr 25 ~prop))
  ([descr trials prop]
  `(fact ~descr
     (tc/quick-check ~trials ~prop) => holds)))
