(ns avi.buffer.content
  (:refer-clojure :exclude [replace])
  (:require [schema.core :as s]))

(defn versioned-mark?
  [mark]
  (= 3 (count mark)))

(def Line (s/both s/Str (s/pred (complement (partial re-find #"\n")))))
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

(def Content
  {:lines [(s/one Line "first line") Line]
   :revision s/Int
   :history {Version HistoryStep}})

(defn- split-lines
  ([text]
   (split-lines text (count text)))
  ([text up-to]
   (loop [line-start 0
          current-offset 0
          lines []]
     (cond
       (= current-offset up-to)
       (conj lines (subs text line-start current-offset))

       (= (get text current-offset) \newline)
       (recur (inc current-offset)
              (inc current-offset)
              (conj lines (subs text line-start current-offset)))

       :else
       (recur line-start (inc current-offset) lines)))))

(defn- text-lines
  [text]
  (let [stopping-point (if (.endsWith text "\n")
                         (dec (count text))
                         (count text))]
    (split-lines text stopping-point)))

(s/defn content :- Content
  [text :- s/Str]
  {:lines (text-lines text)
   :revision 0
   :history {}})

(s/defn before :- [Line]
  [lines :- [Line]
   [end-line end-column] :- Mark]
  (-> lines
    (subvec 0 (dec end-line))
    (conj (subs (get lines (dec end-line)) 0 end-column))))

(s/defn after :- [Line]
  [lines :- [Line]
   [start-line start-column] :- Mark]
  (vec (concat [(subs (get lines (dec start-line)) start-column)]
               (subvec lines start-line))))

(defn join
  ([a b]
   (vec (concat (drop-last a)
                [(str (last a) (first b))]
                (drop 1 b))))
  ([a b c]
   (join (join a b) c)))

(s/defn version-mark :- VersionedMark
  "Creates a versioned mark from a simple mark"
  [{:keys [revision]} :- Content
   mark :- SimpleMark]
  (conj mark revision))

(s/defn unversion-mark :- (s/maybe SimpleMark)
  [{:keys [revision history]} :- Content
   [line column version :as mark] :- Mark]
  (if-not (versioned-mark? mark)
    mark
    (loop [version version
           line line
           column column]
      (let [{[start-line start-column] :start
             [end-line end-column] :end
             :keys [+lines +columns]} (get history version)]
        (cond
          (= version revision)
          [line column]

          (> line end-line)
          (recur (inc version) (+ line +lines) column)

          (and (= line end-line)
               (>= column end-column))
          (recur (inc version) line (+ column +columns))

          (or (> line start-line)
              (and (= line start-line)
                   (> column start-column)))
          nil

          :else
          (recur (inc version) line column))))))

(s/defn replace :- Content
  "Replace text between the `start` mark and the `end` mark with `replacement`.

  `replacement` may contain newlines, and the `start` and `end` marks can span
  lines; therefore, this is the most general content operation which can insert,
  delete, or replace text."
  [{:keys [lines revision] :as content} :- Content
   start :- Mark
   end :- Mark
   replacement :- s/Str]
  (let [replacement-lines (split-lines replacement)]
    (-> content
      (update-in [:revision] inc)
      (update-in [:history] assoc revision {:start start
                                            :end end
                                            :+lines (dec (count replacement-lines))
                                            :+columns (count (last replacement-lines))})
      (assoc :lines (join (before lines start)
                          replacement-lines
                          (after lines end))))))
