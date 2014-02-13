(ns viv.buffer
  (:require [clojure.string :as string]))

(defn open
  [filename]
  {:name filename,
   :lines (string/split (slurp filename) #"\n"),
   :cursor [0 0],
   :last-explicit-j 0})
