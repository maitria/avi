(ns avi.scan-test
  (:require [midje.sweet :refer :all]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as gen']
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.properties :as prop']
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

(defspec advance-at-eof-is-always-nil 100
  (prop'/for-all [lines lines-generator
                  :let [i (dec (count lines))
                        j (dec (count (last lines)))]]
    (nil? (scan/advance [i j] lines))))

(defn- before?
  [[i1 j1] [i2 j2]]
  (or (< i1 i2)
      (and (= i1 i2)
           (< j1 j2))))

(defspec retreat-position-always-decreases 100
  (prop/for-all [{:keys [lines position]} lines-and-position-generator]
    (or (nil? (scan/retreat position lines))
        (before? (scan/retreat position lines) position))))

(defspec advance-position-always-increases 100
  (prop/for-all [{:keys [lines position]} lines-and-position-generator]
    (or (nil? (scan/advance position lines))
        (before? position (scan/advance position lines)))))

(defspec retreat-at-beginning-of-line-goes-to-newline-position 100
  (prop'/for-all [lines lines-generator
                  :when (>= (count lines) 2)
                  i (gen'/bounded-int 1 (dec (count lines)))]
    (= (scan/retreat [i 0] lines)
       [(dec i) (count (get lines (dec i)))])))

(defspec advance-on-last-character-of-any-line-but-last-goes-to-newline-position 100
  (prop'/for-all [lines lines-generator
                  :when (>= (count lines) 2)
                  i (gen'/bounded-int 0 (- (count lines) 2))
                  :let [j (dec (count (get lines i)))]]
    (= (scan/advance [i j] lines) [i (inc j)])))

(defspec retreat-never-skips-a-line 100
  (prop/for-all [{lines :lines [i j] :position} lines-and-position-generator]
    (or (nil? (scan/retreat [i j] lines))
        (= i (first (scan/retreat [i j] lines)))
        (= (dec i) (first (scan/retreat [i j] lines))))))

(defspec advance-never-skips-a-line 100
  (prop/for-all [{lines :lines [i j] :position} lines-and-position-generator]
    (or (nil? (scan/advance [i j] lines))
        (= i (first (scan/advance [i j] lines)))
        (= (inc i) (first (scan/advance [i j] lines))))))
