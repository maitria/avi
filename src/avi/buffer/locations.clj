(ns avi.buffer.locations
  (:require [schema.core :as s]))

(defn versioned-mark?
  [mark]
  (= 3 (count mark)))

(def LineNumber (s/both s/Int (s/pred pos?)))
(def ColumnNumber (s/both s/Int (s/pred (complement neg?))))
(def Version s/Int)

(def Location
  [(s/one LineNumber "LineNumber") 
   (s/one ColumnNumber "Column")])

(def VersionedMark
  [(s/one LineNumber "LineNumber") 
   (s/one ColumnNumber "Column")
   (s/one Version "Version")])

(def Mark
  (s/conditional
    versioned-mark?
    VersionedMark
    
    :else
    Location))

(def HistoryStep
  {:start Location
   :end Location
   :+lines s/Int
   :+columns s/Int})

(def History
  {Version HistoryStep})

(s/defn location<
  [[ai aj] :- Location
   [bi bj] :- Location]
  (or (< ai bi)
      (and (= ai bi)
           (< aj bj))))

(s/defn location<=
  [a :- Location
   b :- Location]
  (or (= a b)
      (location< a b)))

(s/defn location>
  [a :- Location
   b :- Location]
  (location< b a))

(s/defn location>=
  [a :- Location
   b :- Location]
  (or (= a b)
      (location> a b)))
