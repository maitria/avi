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
                [""]))
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

(defn line-count
  [buffer]
  (count (:lines buffer)))

(defn move-to-line
  [buffer i]
  {:pre [(>= i 0) (< i (line-count buffer))]}
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

(defn move-cursor
  [buffer cursor & [j]]
  (-> buffer
      (assoc :cursor cursor)
      (cond-> j (assoc :last-explicit-j j))
      (adjust-viewport-to-contain-cursor)))

(defn last-explicit-j
  [buffer]
  (:last-explicit-j buffer))

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

(defn on-last-line?
  [buffer]
  (let [[i] (cursor buffer)
        line-count (line-count buffer)]
    (= i (dec line-count))))

(defn- clamp-viewport-top
  [{top :viewport-top,
    height :viewport-height,
    :as buffer}
   new-top]
  (let [line-count (line-count buffer)
        max-top (max 0 (- line-count height))]
    (min max-top (max 0 new-top))))

(defn- clamp-cursor-row
  [{top :viewport-top,
    height :viewport-height,
    :as buffer}
   new-top]
  (max 0 (min (dec (line-count buffer)) new-top)))

(defn move-and-scroll-half-page
  [{top :viewport-top,
    height :viewport-height,
    [i] :cursor,
    :as buffer}
   which-way]
  (let [distance (quot height 2)
        direction (case which-way
                    :down +1
                    :up -1)
        scroll-adjust (* direction distance)]
    (-> buffer
        (move-to-line (clamp-cursor-row buffer (+ i scroll-adjust)))
        (scroll (constantly (clamp-viewport-top buffer (+ top scroll-adjust)))))))

(defn cursor-to-bottom-of-viewport
  [{top :viewport-top,
    height :viewport-height,
    :as buffer}]
  (let [bottom-of-viewport (dec (+ top height))
        bottom-of-file (dec (line-count buffer))
        new-line (min bottom-of-file bottom-of-viewport)]
    (move-to-line buffer new-line)))

(defn cursor-to-top-of-viewport
  [{top :viewport-top,
    :as buffer}]
  (move-to-line buffer top))
