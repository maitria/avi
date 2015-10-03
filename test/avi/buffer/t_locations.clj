(ns avi.buffer.t-locations
  (:require [avi.buffer
             [lines :as lines]
             [locations :refer :all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as gen']
            [com.gfredericks.test.chuck.properties :as prop']
            [midje.sweet :refer :all]))

(facts "about comparing simple locations"
  (location< [1 2] [1 4]) => true
  (location< [1 2] [2 2]) => true
  (location< [1 4] [2 2]) => true
  (location<= [1 2] [1 2]) => true)

(def location-generator
  (gen/tuple
    (gen/choose 1 50)
    (gen/choose 0 50)))

(defspec location<-location>-symmetry 25
  (prop/for-all [a location-generator
                 b location-generator]
   (= (location< a b) (location> b a))))

(defspec location<-implies-location<= 25
  (prop'/for-all [a location-generator
                  b location-generator]
   (if (location< a b)
     (location<= a b)
     true)))

(defspec location>-implies-location>= 25
  (prop'/for-all [a location-generator
                  b location-generator]
   (if (location> a b)
     (location>= a b)
     true)))

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

(def line-length-generator
  (gen/vector gen/pos-int 1 35))

(defspec retreat-from-0-0-is-always-nil 100
  (prop/for-all [line-length line-length-generator]
    (nil? (retreat [0 0] line-length))))

(defspec advance-at-eof-is-always-nil 100
  (prop'/for-all [line-length line-length-generator
                  :let [i (dec (count line-length))
                        j (last line-length)]]
    (nil? (advance [i j] #(get line-length %)))))

(defspec retreat-position-always-decreases 100
  (prop/for-all [{:keys [lines position]} lines-and-position-generator]
    (or (nil? (retreat position (lines/line-length lines)))
        (location< (retreat position (lines/line-length lines)) position))))

(defspec advance-position-always-increases 100
  (prop/for-all [{:keys [lines position]} lines-and-position-generator]
    (or (nil? (advance position (lines/line-length lines)))
        (location< position (advance position (lines/line-length lines))))))

(defspec retreat-at-beginning-of-line-goes-to-newline-position 100
  (prop'/for-all [line-length line-length-generator
                  :when (>= (count line-length) 2)
                  i (gen'/bounded-int 1 (dec (count line-length)))]
    (= (retreat [i 0] line-length)
       [(dec i) (line-length (dec i))])))

(defspec advance-on-last-character-of-any-line-but-last-goes-to-newline-position 100
  (prop'/for-all [line-lengths (gen/vector (gen'/bounded-int 1 25))
                  :when (>= (count line-lengths) 2)
                  i (gen'/bounded-int 0 (- (count line-lengths) 2))
                  :let [j (dec (line-lengths i))]]
    (= (advance [i j] line-lengths) [i (inc j)])))

(defspec retreat-never-skips-a-line 100
  (prop/for-all [{lines :lines [i j] :position} lines-and-position-generator]
    (or (nil? (retreat [i j] (lines/line-length lines)))
        (= i (first (retreat [i j] (lines/line-length lines))))
        (= (dec i) (first (retreat [i j] (lines/line-length lines)))))))

(defspec advance-never-skips-a-line 100
  (prop/for-all [{lines :lines [i j] :position} lines-and-position-generator]
    (or (nil? (advance [i j] (lines/line-length lines)))
        (= i (first (advance [i j] (lines/line-length lines))))
        (= (inc i) (first (advance [i j] (lines/line-length lines)))))))

(defspec adjust-for-replacement-passes-locations-before-replacement 100
  (prop'/for-all [[l a b] (gen/fmap sort (gen/vector location-generator 3))
                  :when (not (= l a))
                  line-count (gen/choose 0 35)
                  last-length (gen/choose 0 25)
                  bias (gen/elements [:left :right])]
    (= l (adjust-for-replacement l a b line-count last-length bias))))
