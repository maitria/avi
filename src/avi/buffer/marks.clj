(ns avi.buffer.marks
  (:require [schema.core :as s]))

(defn versioned-mark?
  [mark]
  (= 3 (count mark)))

(def LineNumber (s/both s/Int (s/pred pos?)))
(def ColumnNumber (s/both s/Int (s/pred (complement neg?))))
(def Version s/Int)

(def SimpleMark
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
    SimpleMark))

(def HistoryStep
  {:start SimpleMark
   :end SimpleMark
   :+lines s/Int
   :+columns s/Int})

(def History
  {Version HistoryStep})

(s/defn mark<
  [[ai aj] :- SimpleMark
   [bi bj] :- SimpleMark]
  (or (< ai bi)
      (and (= ai bi)
           (< aj bj))))

(s/defn mark<=
  [a :- SimpleMark
   b :- SimpleMark]
  (or (= a b)
      (mark< a b)))

(s/defn mark>
  [a :- SimpleMark
   b :- SimpleMark]
  (mark< b a))

(s/defn mark>=
  [a :- SimpleMark
   b :- SimpleMark]
  (or (= a b)
      (mark> a b)))

(s/defn version-mark :- VersionedMark
  [revision :- Version
   mark :- SimpleMark]
  (conj mark revision))

(s/defn unversion-mark :- (s/maybe SimpleMark)
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
