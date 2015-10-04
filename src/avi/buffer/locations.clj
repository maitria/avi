(ns avi.buffer.locations
  (:refer-clojure :exclude [replace])
  (:require [schema.core :as s]))

(def ZLineNumber (s/both s/Int (s/pred (complement neg?))))
(def ColumnNumber (s/both s/Int (s/pred (complement neg?))))

(def Location
  [(s/one ZLineNumber "line number (zero-based)")
   (s/one ColumnNumber "column")])

(s/defn location<
  [a :- Location
   b :- Location]
  (< (.compareTo a b) 0))

(s/defn location<=
  [a :- Location
   b :- Location]
  (<= (.compareTo a b) 0))

(s/defn location>
  [a :- Location
   b :- Location]
  (> (.compareTo a b) 0))

(s/defn location>=
  [a :- Location
   b :- Location]
  (>= (.compareTo a b) 0))

(s/defn advance :- (s/maybe Location)
  [[i j] :- Location
   line-length]
  (cond
    (>= j (line-length i))
    (if-not (line-length (inc i))
      nil
      [(inc i) 0])

    :else
    [i (inc j)]))

(s/defn retreat :- (s/maybe Location)
  [[i j] :- Location
   line-length]
  (cond
    (= [i j] [0 0])
    nil

    (>= j 1)
    [i (dec j)]

    :else
    [(dec i) (line-length (dec i))]))

(defn forward
  [pos line-length]
  (lazy-seq
    (when pos
      (cons pos (forward (advance pos line-length) line-length)))))

(defn backward
  [pos line-length]
  (lazy-seq
    (when pos
      (cons pos (backward (retreat pos line-length) line-length)))))

(s/defn forget-location? :- s/Bool
  [a :- Location
   b :- Location
   l :- Location]
  (and (location< a l)
       (location< l b)))

(s/defn adjust-for-replacement :- (s/maybe Location)
  [location :- Location
   a :- Location
   b :- Location
   replacement-line-count :- s/Int
   length-of-last-replacement-line :- s/Int
   bias :- (s/enum :left :right)]
  (cond
    (and (= a b location) (= bias :left))
    location

    (forget-location? a b location)
    nil

    (location<= b location)
    [(-> (first location)
       (- (- (first b) (first a)))
       (+ replacement-line-count))
     (second location)]

    :else
    location))
