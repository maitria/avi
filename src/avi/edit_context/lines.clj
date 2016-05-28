(ns avi.edit-context.lines
  (:refer-clojure :exclude [replace])
  (:require [packthread.core :refer :all]
            [avi.pervasive :refer :all]
            [avi.edit-context.locations :as l]))

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

(defn content
  [text]
  (text-lines text))

(defn before
  [lines [i j]]
  (-> lines
    (subvec 0 i)
    (conj (subs-with-spaces (get lines i) 0 j))))

(defn after
  [lines [start-line start-column]]
  (vec (concat [(subs-with-spaces (get lines start-line) start-column)]
               (subvec lines (inc start-line)))))

(defn join
  ([a b]
   (vec (concat (drop-last a)
                [(str (last a) (first b))]
                (drop 1 b))))
  ([a b c]
   (join (join a b) c)))

(defn replace
  "Replace text between the `start` location and the `end` location with
  `replacement`.

  `replacement` may contain newlines, and the `start` and `end` locations can
  span lines; therefore, this is the most general content operation which can
  insert, delete, or replace text."
  [lines a b replacement]
  (let [[start end] (sort [a b])]
    (join (before lines start)
          (split-lines replacement)
          (after lines end))))

(defn line-length
  "Convert a vector of lines to a line length function."
  [lines]
  (fn [i]
    (some-> lines (get i) count)))
