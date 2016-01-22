(ns avi.spec-runner
  (:require [midje.sweet :refer :all])
  (:require [avi.test-helpers :refer :all]))

(def ^:private tabular-quality-names
  '{content ?content
    after   ?after
    point   ?point})

(defn- table-headings
  [spec]
  (map tabular-quality-names (take-while tabular-quality-names spec)))

(defmacro facts-about
  [description & spec]
  `(tabular
     (facts ~description
       (editor :editing ~'?content :after ~'?after) => (point ~'?point))
     ~@(table-headings spec)
     ~@(drop-while tabular-quality-names spec)))
