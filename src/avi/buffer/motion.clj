(ns avi.buffer.motion
  "Primitives for moving the point."
  (:require [avi.beep :as beep]
            [avi.buffer
              [lines :as lines]
              [locations :as l]
              [transactions :as t]]
            [avi.buffer.motion
             [goto]
             [resolve :as resolve]]
            [packthread.core :refer :all]))

(defn adjust-column-to-line
  [{:keys [lines] [i j] :point :as buffer}]
  (+> buffer
    (let [line-length (count (get lines i))
          j' (max 0 (min j (dec line-length)))]
      (assoc :point [i j']))))

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

(defn adjust-point-within-line
  [{[i j] :point :keys [lines] :as buffer}]
  (+> buffer
    (let [length (count (get lines i))]
      (assoc :point [i (max 0 (min j (dec length)))]))))

(defn move-point
  [buffer [_ [_ motion-j] :as motion]]
  (+> buffer
    (let [j-is-last-explicit? (= motion-j :last-explicit)
          [i j :as pos] (resolve/resolve-motion buffer motion)]
      (if-not pos
        beep/beep)
      (when pos
        (assoc :point pos)
        (if-not j-is-last-explicit?
          (assoc :last-explicit-j j))
        adjust-point-within-line
        adjust-viewport-to-contain-point))))

(defn delete
  [{start :point :as buffer} motion]
  (+> buffer
    (let [end (resolve/resolve-motion buffer motion)]
      t/start-transaction
      (update-in [:lines] lines/replace start end "")
      t/commit)))
