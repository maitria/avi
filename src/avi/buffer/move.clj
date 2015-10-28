(ns avi.buffer.move
  "Primitives for moving the point."
  (:require [packthread.core :refer :all]))

(defn adjust-viewport-to-contain-point
  [buffer]
  (+> buffer
    (let [height (:viewport-height buffer)
          viewport-top (:viewport-top buffer)
          viewport-bottom (dec (+ viewport-top height))
          [point-i] (:point buffer)]
      (cond
        (< point-i viewport-top)
        (assoc :viewport-top point-i)

        (> point-i viewport-bottom)
        (assoc :viewport-top (inc (- point-i height)))))))

(defn viewport-middle
  [{top :viewport-top,
    height :viewport-height,
    :keys [lines],
    :as buffer}]
  (let [middle-of-viewport (dec (+ top (quot height 2)))
        middle-of-file (quot (dec (count lines)) 2)
        middle (min middle-of-viewport middle-of-file)]
    middle))

(defn j-within-line
  [{:keys [lines last-explicit-j]} i]
  (let [j last-explicit-j
        line-length (count (get lines i))
        j-not-after-end (min (dec line-length) j)
        j-within-line (max 0 j-not-after-end)]
    j-within-line))

(defn index-of-first-non-blank
  [string]
  (let [leading-space-count (count (re-find #"^\s*" string))
        all-spaces? (and (> leading-space-count 0)
                         (= leading-space-count (count string)))]
    (if all-spaces?
      (dec leading-space-count)
      leading-space-count)))

(defn move-point
  [{:keys [lines] :as buffer} [i j]]
  (+> buffer
    (let [i (case i
              :current         (get-in buffer [:point 0])
              :viewport-middle (viewport-middle buffer)
              i)
          explicit? (not= j :last-explicit)
          j (case j
              :end-of-line     (max 0 (dec (count (get lines i))))
              :first-non-blank (index-of-first-non-blank (get lines i))
              :last-explicit   (j-within-line buffer i)
              j)]
      (assoc :point [i j])
      (if explicit?
        (assoc :last-explicit-j j))
      adjust-viewport-to-contain-point)))
