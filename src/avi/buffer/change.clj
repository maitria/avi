(ns avi.buffer.change
  (:require [avi.buffer
               [lines :as lines]
               [locations :as l]]
            [packthread.core :refer :all]
            [schema.core :as s]))

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
        (assoc :point new-point :last-explicit-j j)))))
