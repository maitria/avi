(ns viv.buffer
  (:require [clojure.string :as string]))

(defn open
  [filename]
  {:name filename,
   :lines (string/split (slurp filename) #"\n"),
   :cursor [0 0],
   :last-explicit-j 0})

(defn cursor
  [buffer]
  (:cursor buffer))

(defn last-explicit-j
  [buffer]
  (:last-explicit-j buffer))

(defn line
  [buffer i]
  (get-in buffer [:lines i]))

(defn lines
  [buffer]
  (count (:lines buffer)))
