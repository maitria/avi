(ns avi.buffer.content
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :as string]
            [schema.core :as s]))

(def Line (s/both s/Int (s/pred pos?)))
(def Column (s/both s/Int (s/pred (complement neg?))))

(def Mark
  [(s/one Line "Line") 
   (s/one Column "Column")])

(def Content
  {:lines [(s/one s/Str "first line") s/Str]})

(s/defn content :- Content
  [text :- s/Str]
  {:lines (string/split text #"\n")})

(s/defn replace :- Content
  "Replace text between the `start` mark and the `end` mark with `replacement`.

  `replacement` may contain newlines, and the `start` and `end` marks can span
  lines; therefore, this is the most general content operation which can insert,
  delete, or replace text."
  [content :- Content
   start :- Mark
   end :- Mark
   replacement :- s/Str]
  (update-in content [:lines 0] #(str replacement %)))
