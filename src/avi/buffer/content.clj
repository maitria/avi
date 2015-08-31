(ns avi.buffer.content
  (:require [clojure.string :as string]
            [schema.core :as s]))

(def Content
  {:lines [s/Str]})

(s/defn content :- Content
  [text :- s/Str]
  {:lines (string/split text #"\n")})
