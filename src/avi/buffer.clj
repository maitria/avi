(ns avi.buffer
  (:require [clojure.string :as string]))

(defn open
  [filename size]
  {:name filename,
   :lines (string/split (slurp filename) #"\n"),
   :viewport-offset [0 0]
   :viewport-size size
   :cursor [0 0],
   :last-explicit-j 0})

(defn cursor
  [buffer]
  (:cursor buffer))

(defn with-cursor
  [buffer [cursor-i cursor-j :as cursor] & [j]]
  (let [[height] (:viewport-size buffer)]
    (-> buffer
        (assoc :cursor cursor)
        (cond->
          j
          (assoc :last-explicit-j j)

          (>= cursor-i height)
          (assoc :viewport-offset [(inc (- cursor-i height)) 0])))))

(defn last-explicit-j
  [buffer]
  (:last-explicit-j buffer))

(defn line
  [buffer i]
  (get-in buffer [:lines i]))

(defn lines
  [buffer]
  (count (:lines buffer)))
