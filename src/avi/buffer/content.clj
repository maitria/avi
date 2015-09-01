(ns avi.buffer.content
  (:refer-clojure :exclude [replace])
  (:require [schema.core :as s]))

(def Line (s/both s/Int (s/pred pos?)))
(def Column (s/both s/Int (s/pred (complement neg?))))

(def Mark
  [(s/one Line "Line") 
   (s/one Column "Column")])

(def Content
  {:lines [(s/one s/Str "first line") s/Str]})

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

(s/defn replace :- Content
  "Replace text between the `start` mark and the `end` mark with `replacement`.

  `replacement` may contain newlines, and the `start` and `end` marks can span
  lines; therefore, this is the most general content operation which can insert,
  delete, or replace text."
  [content :- Content
   [_ column] :- Mark
   end :- Mark
   replacement :- s/Str]
  (update-in content [:lines 0] #(str
                                   (subs % 0 column)
                                   replacement
                                   (subs % column))))
