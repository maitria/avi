(ns avi.edit-context.change
  (:require [avi.edit-context
               [lines :as lines]
               [locations :as l]]
            [packthread.core :refer :all]))

(defn adjust-viewport-to-contain-point
  [edit-context]
  (+> edit-context
    (let [height (:viewport-height edit-context)
          viewport-top (:viewport-top edit-context)
          viewport-bottom (dec (+ viewport-top height))
          [point-i] (:point edit-context)]
      (cond
        (< point-i viewport-top)
        (assoc :viewport-top point-i)

        (> point-i viewport-bottom)
        (assoc :viewport-top (inc (- point-i height)))))))

(defn change
  "All content changes happen through me!"
  [{:keys [point] :as edit-context} a b replacement bias]
  (+> edit-context
    (let [[_ j :as new-point] (l/adjust-for-replacement point a b replacement bias)]
      (update-in [:avi.document/lines] lines/replace a b replacement)
      (if new-point
        (assoc :point new-point :last-explicit-j j))
      adjust-viewport-to-contain-point)))
