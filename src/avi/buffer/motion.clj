(ns avi.buffer.motion
  "Primitives for moving the point."
  (:require [avi.buffer
              [locations :as l]]
            [packthread.core :refer :all]))

(defmulti resolve-motion
  (fn [_ [motion-type]]
    motion-type))

(require '[avi.buffer.motion to])

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

(defn move-point
  [buffer [_ [_ motion-j] :as motion]]
  (+> buffer
    (let [j-is-last-explicit? (= motion-j :last-explicit)
          [i j] (resolve-motion buffer motion)]
      (assoc :point [i j])
      (if-not j-is-last-explicit?
        (assoc :last-explicit-j j))
      adjust-viewport-to-contain-point)))
