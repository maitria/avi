(ns avi.buffer.content
  (:refer-clojure :exclude [replace])
  (:require [schema.core :as s]
            [avi.buffer.marks :as marks]))

(def Line (s/both s/Str (s/pred (complement (partial re-find #"\n")))))

(def Content
  {:lines [(s/one Line "first line") Line]
   :revision s/Int
   :history marks/History})

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
   [end-line end-column] :- marks/Mark]
  (-> lines
    (subvec 0 (dec end-line))
    (conj (subs (get lines (dec end-line)) 0 end-column))))

(s/defn after :- [Line]
  [lines :- [Line]
   [start-line start-column] :- marks/Mark]
  (vec (concat [(subs (get lines (dec start-line)) start-column)]
               (subvec lines start-line))))

(defn join
  ([a b]
   (vec (concat (drop-last a)
                [(str (last a) (first b))]
                (drop 1 b))))
  ([a b c]
   (join (join a b) c)))

(s/defn version-mark :- marks/VersionedMark
  "Creates a versioned mark from a simple mark"
  [{:keys [revision]} :- Content
   mark :- marks/SimpleMark]
  (marks/version-mark revision mark))

(s/defn unversion-mark :- (s/maybe marks/SimpleMark)
  [{:keys [revision history]} :- Content
   mark :- marks/Mark]
  (marks/unversion-mark revision history mark))

(s/defn replace :- Content
  "Replace text between the `start` mark and the `end` mark with `replacement`.

  `replacement` may contain newlines, and the `start` and `end` marks can span
  lines; therefore, this is the most general content operation which can insert,
  delete, or replace text."
  [{:keys [lines revision] :as content} :- Content
   start :- marks/Mark
   end :- marks/Mark
   replacement :- s/Str]
  (let [replacement-lines (split-lines replacement)
        [start-line start-column :as start] (unversion-mark content start)
        [end-line end-column :as end] (unversion-mark content end)]
    (-> content
      (update-in [:revision] inc)
      (update-in [:history] assoc revision {:start start
                                            :end end
                                            :+lines (- (dec (count replacement-lines))
                                                       (- end-line start-line))
                                            :+columns (- (count (last replacement-lines))
                                                         (- end-column start-column))})
      (assoc :lines (join (before lines start)
                          replacement-lines
                          (after lines end))))))
