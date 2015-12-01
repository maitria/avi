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

(defmulti adjust-for-motion-kind
  "Per vim docs, \"inclusive\" motions include the last character of the range
  (which we exclude by default because of the way we track the point).
  \"Exclusive\" motions do what we normally do, and \"linewise\" motions
  affect all lines they touch.

  Linewise gets difficult: we want to take either a preceeding _or_ a trailing
  newline with the change or deletion _if we have one_."
  (fn [range lines kind] kind))

(defmethod adjust-for-motion-kind :exclusive
  [range _ _]
  range)

(defmethod adjust-for-motion-kind :inclusive
  [[start [ei ej]] _ _]
  [start [ei (inc ej)]])

(defmethod adjust-for-motion-kind :linewise
  [[[si sj] [ei ej]] lines _]
  (cond
    (and (not (get lines (dec si)))
         (not (get lines (inc ei))))
    [[0 0] [(dec (count lines)) (count (peek lines))]]

    (get lines (inc ei))
    [[si 0] [(inc ei) 0]]

    :else
    [[(dec si) (count (get lines (dec si)))]
     [ei (count (get lines ei))]]))

(defn resolve-range
  [{:keys [lines point] :as buffer} motion kind]
  (when-let [pos (resolve/resolve-motion buffer motion)]
    (-> [point pos]
      sort
      (adjust-for-motion-kind lines kind))))

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

(defmulti invoke-motion
  (fn [buffer params]
    (:operator params)))

(defmethod invoke-motion :move-point
  [buffer {:keys [motion]}]
  (move-point buffer motion))

(defmethod invoke-motion :delete
  [buffer {:keys [motion kind]}]
  (delete buffer motion kind))
