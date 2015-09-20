(ns avi.buffer.locations
  (:require [schema.core :as s]))

(def LineNumber (s/both s/Int (s/pred (complement neg?))))
(def ColumnNumber (s/both s/Int (s/pred (complement neg?))))

(def Location
  [(s/one LineNumber "LineNumber") 
   (s/one ColumnNumber "Column")])

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
