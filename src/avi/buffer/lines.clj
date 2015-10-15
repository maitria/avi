(ns avi.buffer.lines
  (:refer-clojure :exclude [replace])
  (:require [schema.core :as s]
            [avi.buffer.locations :as l]))

(def Line (s/both s/Str (s/pred (complement (partial re-find #"\n")))))
(def Lines [(s/one Line "first line") Line])

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

(s/defn content :- Lines
  [text :- s/Str]
  (text-lines text))

(defn subs-with-spaces
  ([s start]
   (if (> start (count s))
     ""
     (subs s start)))
  ([s start end]
   {:post [(= (count %) (- end start))]}
   (let [s-end (min end (count s))]
     (apply str
            (subs s start s-end)
            (repeat (- end s-end) \space)))))

(s/defn before :- [Line]
  [lines :- [Line]
   [i j] :- l/Location]
  (-> lines
    (subvec 0 i)
    (conj (subs-with-spaces (get lines i) 0 j))))

(s/defn after :- [Line]
  [lines :- [Line]
   [start-line start-column] :- l/Location]
  (vec (concat [(subs-with-spaces (get lines start-line) start-column)]
               (subvec lines (inc start-line)))))

(defn join
  ([a b]
   (vec (concat (drop-last a)
                [(str (last a) (first b))]
                (drop 1 b))))
  ([a b c]
   (join (join a b) c)))

(s/defn replace :- Lines
  "Replace text between the `start` location and the `end` location with
  `replacement`.

  `replacement` may contain newlines, and the `start` and `end` locations can
  span lines; therefore, this is the most general content operation which can
  insert, delete, or replace text."
  [lines :- Lines
   a :- l/Location
   b :- l/Location
   replacement :- s/Str]
  (let [[start end] (sort [a b])]
    (join (before lines start)
          (split-lines replacement)
          (after lines end))))

(defn line-length
  "Convert a vector of lines to a line length function."
  [lines]
  (fn [i]
    (some-> lines (get i) count)))
