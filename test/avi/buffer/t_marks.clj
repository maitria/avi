(ns avi.buffer.t-marks
  (:require [avi.buffer.marks :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as gen']
            [com.gfredericks.test.chuck.properties :as prop']
            [midje.sweet :refer :all]))

(facts "about comparing simple marks"
  (mark< [1 2] [1 4]) => true
  (mark< [1 2] [2 2]) => true
  (mark< [1 4] [2 2]) => true
  (mark<= [1 2] [1 2]) => true)

(def simple-mark-generator
  (gen/tuple
    (gen/choose 1 50)
    (gen/choose 0 50)))

(defspec mark<-mark>-symmetry 25
  (prop/for-all [a simple-mark-generator
                 b simple-mark-generator]
   (= (mark< a b) (mark> b a))))

(defspec mark<-implies-mark<= 25
  (prop'/for-all [a simple-mark-generator
                  b simple-mark-generator]
   (if (mark< a b)
     (mark<= a b)
     true)))

(defspec mark>-implies-mark>= 25
  (prop'/for-all [a simple-mark-generator
                  b simple-mark-generator]
   (if (mark> a b)
     (mark>= a b)
     true)))
