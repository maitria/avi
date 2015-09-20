(ns avi.buffer.marks
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

(s/defn mark<
  [[ai aj] :- Location
   [bi bj] :- Location]
  (or (< ai bi)
      (and (= ai bi)
           (< aj bj))))

(s/defn mark<=
  [a :- Location
   b :- Location]
  (or (= a b)
      (mark< a b)))

(s/defn mark>
  [a :- Location
   b :- Location]
  (mark< b a))

(s/defn mark>=
  [a :- Location
   b :- Location]
  (or (= a b)
      (mark> a b)))

(s/defn version-mark :- VersionedMark
  [revision :- Version
   mark :- Location]
  (conj mark revision))

(s/defn unversion-mark :- (s/maybe Location)
  [revision :- Version
   history :- History
   [line column version :as mark] :- Mark]
  (if-not (versioned-mark? mark)
    mark
    (loop [version version
           line line
           column column]
      (let [{[end-line end-column :as end] :end
             :keys [start +lines +columns]} (get history version)]
        (cond
          (= version revision)
          [line column]

          (> line end-line)
          (recur (inc version) (+ line +lines) column)

          (mark> [line column] end)
          (recur (inc version) line (+ column +columns))

          (mark> [line column] start)
          nil

          :else
          (recur (inc version) line column))))))
