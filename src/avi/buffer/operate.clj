(ns avi.buffer.operate
  "Primitives for moving the point."
  (:require [avi.beep :as beep]
            [avi.buffer
              [change :as c]
              [lines :as lines]
              [locations :as l]
              [transactions :as t]]
            [avi.buffer.operate
             [goto]
             [resolve :as resolve]
             [word]]
            [packthread.core :refer :all]))

(defmulti operate
  (fn [buffer params]
    (:operator params)))

(defn clamped-j
  [{[i] :point,
    :keys [lines]}
   j]
  (max 0 (min j (dec (count (get lines i))))))

(defn clamp-point-j
  [{[i j] :point,
    :as buffer}]
  (assoc buffer :point [i (clamped-j buffer j)]))

(defn explicit-column?
  [{motion :motion}]
  (not (->> motion
         flatten
         (filter (partial = :last-explicit))
         seq)))

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
  [{:keys [lines point] :as buffer} {:keys [kind] :as operation}]
  (when-let [pos (resolve/resolve-motion buffer operation)]
    (-> [point pos]
      sort
      (adjust-for-motion-kind lines kind))))

(defn delete
  [{start :point :keys [lines] :as buffer} operation]
  (+> buffer
    (if-let [[start end] (resolve-range buffer operation)]
      (do
        t/start-transaction
        (c/change start end "" :left)
        t/commit
        (operate {:operator :move-point
                  :motion [:goto start]}))
      beep/beep)))

(defmethod operate :move-point
  [buffer operation]
  (+> buffer
    (let [[i j :as pos] (resolve/resolve-motion buffer operation)]
      (if-not pos
        beep/beep)
      (when pos
        (assoc :point pos)
        (if (explicit-column? operation)
          (assoc :last-explicit-j j))
        clamp-point-j
        c/adjust-viewport-to-contain-point))))

(defmethod operate :delete
  [buffer params]
  (delete buffer params))
