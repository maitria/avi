(ns avi.buffer.change
  (:require [avi.buffer
               [lines :as lines]
               [locations :as l]]
            [packthread.core :refer :all]
            [schema.core :as s]))

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

(s/defn change
  "All content changes happen through me!"
  [{:keys [point] :as buffer}
   a :- l/Location
   b :- l/Location
   replacement :- s/Str
   bias :- l/AdjustmentBias]
  (+> buffer
    (let [[_ j :as new-point] (l/adjust-for-replacement point a b replacement bias)]
      (update-in [:lines] lines/replace a b replacement)
      (if new-point
        (assoc :point new-point :last-explicit-j j))
      adjust-viewport-to-contain-point)))