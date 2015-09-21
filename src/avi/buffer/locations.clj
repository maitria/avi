(ns avi.buffer.locations
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

(defn advance
  [[i j] line-length]
  (cond
    (>= j (line-length i))
    (if-not (line-length (inc i))
      nil
      [(inc i) 0])

    :else
    [i (inc j)]))

(defn retreat
  [[i j] line-length]
  (cond
    (= [i j] [0 0])
    nil

    (>= j 1)
    [i (dec j)]

    :else
    [(dec i) (line-length (dec i))]))
