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

(defn- table-data
  [spec]
  (drop-while tabular-quality-names spec))

(defn- editor-invocation
  [spec]
  `(editor :editing ~'?content :after ~'?after))

(defmacro facts-about
  [description & spec]
  `(tabular
     (facts ~description
       ~(editor-invocation spec) => (point ~'?point))
     ~@(table-headings spec)
     ~@(table-data spec)))
