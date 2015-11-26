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
  [buffer [_ [_ motion-j] :as motion] & [_]]
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

(defn resolve-range
  [{:keys [lines point] :as buffer} motion kind]
  (when-let [pos (resolve/resolve-motion buffer motion)]
    (let [[start end] (sort [point pos])
          [start end] (case kind
                        :exclusive [start end]
                        :inclusive [start [(first end) (inc (second end))]]
                        :linewise  (let [[si sj] start
                                         [ei ej] end]
                                     (cond
                                       (and (not (get lines (dec si)))
                                            (not (get lines (inc ei))))
                                       [[0 0] [(dec (count lines)) (count (peek lines))]]

                                       (get lines (inc ei))
                                       [[si 0] [(inc ei) 0]]

                                       :else
                                       [[(dec si) (count (get lines (dec si)))]
                                        [ei (count (get lines ei))]])))]
      [start end])))

(defn delete
  [{start :point :keys [lines] :as buffer} motion kind]
  (+> buffer
    (if-let [[start end] (resolve-range buffer motion kind)]
      (do
        t/start-transaction
        (c/change start end "" :left)
        t/commit
        (move-point [:goto start]))
      beep/beep)))
