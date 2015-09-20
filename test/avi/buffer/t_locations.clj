(ns avi.buffer.t-locations
  (:require [avi.buffer.locations :refer :all]
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

(def simple-location-generator
  (gen/tuple
    (gen/choose 1 50)
    (gen/choose 0 50)))

(defspec location<-location>-symmetry 25
  (prop/for-all [a simple-location-generator
                 b simple-location-generator]
   (= (location< a b) (location> b a))))

(defspec location<-implies-location<= 25
  (prop'/for-all [a simple-location-generator
                  b simple-location-generator]
   (if (location< a b)
     (location<= a b)
     true)))

(defspec location>-implies-location>= 25
  (prop'/for-all [a simple-location-generator
                  b simple-location-generator]
   (if (location> a b)
     (location>= a b)
     true)))
