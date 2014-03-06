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

(defn line
  [buffer i]
  (get-in buffer [:lines i]))

(defn j-within-line
  [buffer i]
  (let [j (:last-explicit-j buffer)
        line-length (count (line buffer i))
        j-not-after-end (min (dec line-length) j)
        j-within-line (max 0 j-not-after-end)]
    j-within-line))

(defn move-to-line
  [buffer i]
  (-> buffer
      (assoc :cursor [i (j-within-line buffer i)])
      (adjust-viewport-to-contain-cursor)))

(defn- adjust-cursor-to-viewport
  [buffer]
  (let [height (:viewport-height buffer)
        viewport-top (:viewport-top buffer)
        viewport-bottom (dec (+ viewport-top height))
        [cursor-i] (:cursor buffer)]
    (cond-> buffer
      (< cursor-i viewport-top)
      (move-to-line viewport-top)

      (> cursor-i viewport-bottom)
      (move-to-line viewport-bottom))))

(defn with-cursor
  [buffer cursor & [j]]
  (-> buffer
      (assoc :cursor cursor)
      (cond-> j (assoc :last-explicit-j j))
      (adjust-viewport-to-contain-cursor)))

(defn last-explicit-j
  [buffer]
  (:last-explicit-j buffer))

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

(defn scroll-down-half-page
  [{top :viewport-top,
    height :viewport-height,
    [i] :cursor,
    :as buffer}]
  (let [distance (quot height 2)
        line-count (lines buffer)
        max-top (max 0 (- line-count height))
        new-top (min max-top (+ top distance))
        new-i (+ i distance)]
    (-> buffer
        (move-to-line new-i)
        (scroll (constantly new-top)))))

(defn on-last-line?
  [buffer]
  (let [[i] (cursor buffer)
        line-count (lines buffer)]
    (= i (dec line-count))))
