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
  (let [text-end (if (.endsWith text "\n")
                   (dec (count text))
                   (count text))]
    (loop [line-start 0
           line-end 0
           lines []]
      (cond
        (= line-end text-end)
        (conj lines (subs text line-start line-end))

        (= (get text line-end) \newline)
        (recur (inc line-end) (inc line-end) (conj lines (subs text line-start line-end)))

        :else
        (recur line-start (inc line-end) lines)))))

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
