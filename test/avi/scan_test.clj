(ns avi.scan-test
  (:require [midje.sweet :refer :all]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as gen']
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [avi.scan :as scan]))

(def line-generator (gen/such-that #(= -1 (.indexOf % "\n")) gen/string-ascii))
(def lines-generator (gen/such-that #(not (zero? (count %))) (gen/vector line-generator)))
(def lines-and-position-generator
  (gen'/for [lines lines-generator
             i-base gen/pos-int
             :let [i (mod i-base (count lines))]
             j-base gen/pos-int
             :let [j (mod j-base (inc (count (get lines i))))]]
    {:lines lines
     :position [i j]}))

(defspec retreat-from-0-0-is-always-nil 100
  (prop/for-all [lines lines-generator]
    (nil? (scan/retreat [0 0] lines))))

(defn- before?
  [[i1 j1] [i2 j2]]
  (or (< i1 i2)
      (and (= i1 i2)
           (< j1 j2))))

(defspec retreat-position-always-decreases 100
  (prop/for-all [{:keys [lines position]} lines-and-position-generator]
    (or (nil? (scan/retreat position lines))
        (before? (scan/retreat position lines) position))))
