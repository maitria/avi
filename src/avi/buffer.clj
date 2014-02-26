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

(defn- adjust-viewport-to-contain-cursor
  [buffer]
  (let [[height] (:viewport-size buffer)
        [viewport-offset-i] (:viewport-offset buffer)
        bottom-line (dec (+ viewport-offset-i height))
        [cursor-i] (:cursor buffer)]
    (cond-> buffer
      (< cursor-i viewport-offset-i)
      (assoc :viewport-offset [cursor-i 0])

      (> cursor-i bottom-line)
      (assoc :viewport-offset [(inc (- cursor-i height)) 0]))))

(defn with-cursor
  [buffer cursor & [j]]
  (-> buffer
      (assoc :cursor cursor)
      (cond-> j (assoc :last-explicit-j j))
      (adjust-viewport-to-contain-cursor)))

(defn last-explicit-j
  [buffer]
  (:last-explicit-j buffer))

(defn line
  [buffer i]
  (get-in buffer [:lines i]))

(defn lines
  [buffer]
  (count (:lines buffer)))
