(ns avi.buffer.content
  (:refer-clojure :exclude [replace])
  (:require [schema.core :as s]))

(def Line (s/both s/Str (s/pred (complement (partial re-find #"\n")))))
(def LineNumber (s/both s/Int (s/pred pos?)))
(def ColumnNumber (s/both s/Int (s/pred (complement neg?))))

(def Mark
  [(s/one LineNumber "LineNumber") 
   (s/one ColumnNumber "Column")])

(def Content
  {:lines [(s/one Line "first line") Line]})

(defn- split-lines
  [text]
  (let [stopping-point (if (.endsWith text "\n")
                         (dec (count text))
                         (count text))]
    (loop [line-start 0
           current-offset 0
           lines []]
      (cond
        (= current-offset stopping-point)
        (conj lines (subs text line-start current-offset))

        (= (get text current-offset) \newline)
        (recur (inc current-offset)
               (inc current-offset)
               (conj lines (subs text line-start current-offset)))

        :else
        (recur line-start (inc current-offset) lines)))))

(s/defn content :- Content
  [text :- s/Str]
  {:lines (split-lines text)})

(s/defn lines-upto :- [Line]
  [lines :- [Line]
   [end-line end-column] :- Mark]
  (-> lines
    (subvec 0 (dec end-line))
    (conj (subs (get lines (dec end-line)) 0 end-column))))

(s/defn lines-from :- [Line]
  [lines :- [Line]
   [start-line start-column] :- Mark]
  (vec (concat [(subs (get lines (dec start-line)) start-column)]
               (subvec lines start-line))))

(defn join-line-sets
  ([a b]
   (vec (concat (drop-last a)
                [(str (last a) (first b))]
                (drop 1 b))))
  ([a b c]
   (join-line-sets (join-line-sets a b) c)))

(defn- replacement-lines
  [text]
  (let [stopping-point (count text)]
    (loop [line-start 0
           current-offset 0
           lines []]
      (cond
        (= current-offset stopping-point)
        (conj lines (subs text line-start current-offset))

        (= (get text current-offset) \newline)
        (recur (inc current-offset)
               (inc current-offset)
               (conj lines (subs text line-start current-offset)))

        :else
        (recur line-start (inc current-offset) lines)))))

(s/defn replace :- Content
  "Replace text between the `start` mark and the `end` mark with `replacement`.

  `replacement` may contain newlines, and the `start` and `end` marks can span
  lines; therefore, this is the most general content operation which can insert,
  delete, or replace text."
  [{:keys [lines] :as content} :- Content
   start :- Mark
   end :- Mark
   replacement :- s/Str]
  (assoc content :lines (join-line-sets
                          (lines-upto lines start)
                          (replacement-lines replacement)
                          (lines-from lines end))))
