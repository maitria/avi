(ns avi.buffer
  (:import [java.io FileNotFoundException])
  (:require [clojure.string :as string]))

(defn open
  [filename height]
  {:name filename,
   :lines (if filename
            (try
              (string/split (slurp filename) #"\n")
              (catch FileNotFoundException e
                [""]
                ))
            [""]),
   :viewport-top 0
   :viewport-height height
   :cursor [0 0],
   :last-explicit-j 0})

(defn cursor
  [buffer]
  (:cursor buffer))

(defn- adjust-viewport-to-contain-cursor
  [buffer]
  (let [height (:viewport-height buffer)
        viewport-top (:viewport-top buffer)
        viewport-bottom (dec (+ viewport-top height))
        [cursor-i] (:cursor buffer)]
    (cond-> buffer
      (< cursor-i viewport-top)
      (assoc :viewport-top cursor-i)

      (> cursor-i viewport-bottom)
      (assoc :viewport-top (inc (- cursor-i height))))))

(defn- adjust-cursor-to-viewport
  [buffer]
  (let [height (:viewport-height buffer)
        viewport-top (:viewport-top buffer)
        viewport-bottom (dec (+ viewport-top height))
        [cursor-i] (:cursor buffer)]
    (cond-> buffer
      (< cursor-i viewport-top)
      (assoc-in [:cursor 0] viewport-top)

      (> cursor-i viewport-bottom)
      (assoc-in [:cursor 0] viewport-bottom))))

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

(defn resize
  [buffer height]
  (-> buffer
      (assoc :viewport-height height)
      (adjust-viewport-to-contain-cursor)))

(defn scroll
  [buffer scroll-fn]
  (-> buffer
      (update-in [:viewport-top] scroll-fn)
      (adjust-cursor-to-viewport)))
