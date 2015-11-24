(ns avi.buffer.motion
  "Primitives for moving the point."
  (:require [avi.beep :as beep]
            [avi.buffer
              [change :as c]
              [lines :as lines]
              [locations :as l]
              [transactions :as t]]
            [avi.buffer.motion
             [goto]
             [resolve :as resolve]]
            [packthread.core :refer :all]))

(defn clamped-j
  [{[i] :point,
    :keys [lines]}
   j]
  (max 0 (min j (dec (count (get lines i))))))

(defn clamp-point-j
  [{[i j] :point,
    :as buffer}]
  (assoc buffer :point [i (clamped-j buffer j)]))

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
        clamp-point-j
        c/adjust-viewport-to-contain-point))))

(defn delete
  [{start :point :keys [lines] :as buffer} motion]
  (+> buffer
    (if-let [end (resolve/resolve-motion buffer motion)]
      (let [moving-left? (l/location< start end)
            end' (if moving-left?
                   [(first end) (inc (second end))]
                   end)]
        t/start-transaction
        (c/change start end' "" :left)
        t/commit
        (move-point [:goto (first (sort [start end']))])))))