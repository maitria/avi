(ns avi.buffer.content
  (:refer-clojure :exclude [replace])
  (:require [schema.core :as s]
            [avi.buffer.locations :as l]))

(def Line (s/both s/Str (s/pred (complement (partial re-find #"\n")))))
(def Lines [(s/one Line "first line") Line])
(def Content {:lines Lines})

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
  {:lines (text-lines text)})

(s/defn before :- [Line]
  [lines :- [Line]
   [end-line end-column] :- l/Location]
  (-> lines
    (subvec 0 (dec end-line))
    (conj (subs (get lines (dec end-line)) 0 end-column))))

(s/defn after :- [Line]
  [lines :- [Line]
   [start-line start-column] :- l/Location]
  (vec (concat [(subs (get lines (dec start-line)) start-column)]
               (subvec lines start-line))))

(defn join
  ([a b]
   (vec (concat (drop-last a)
                [(str (last a) (first b))]
                (drop 1 b))))
  ([a b c]
   (join (join a b) c)))

(s/defn replace :- Content
  "Replace text between the `start` location and the `end` location with
  `replacement`.

  `replacement` may contain newlines, and the `start` and `end` locations can
  span lines; therefore, this is the most general content operation which can
  insert, delete, or replace text."
  [{:keys [lines] :as content} :- Content
   [start-line start-column :as start] :- l/Location
   [end-line end-column :as end] :- l/Location
   replacement :- s/Str]
  (assoc content :lines (join (before lines start)
                              (split-lines replacement)
                              (after lines end))))
