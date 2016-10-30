(ns avi.edit-context.operate
  "Primitives for moving the point."
  (:require [avi.beep :as beep]
            [avi.edit-context
              [change :as c]
              [lines :as lines]
              [locations :as l]
              [transactions :as t]]
            [avi.edit-context.operate
             [goto]
             [resolve :as resolve]
             [word]]
            [packthread.core :refer :all]))

(defmulti operate
  (fn [edit-context params]
    (:operator params)))

(defn clamped-j
  [{[i] :point,
    :keys [:avi.document/lines]}
   j]
  (max 0 (min j (dec (count (get lines i))))))

(defn clamp-point-j
  [{[i j] :point,
    :as edit-context}]
  (assoc edit-context :point [i (clamped-j edit-context j)]))

(defmulti adjust-for-span
  "Per vim docs, \"inclusive\" motions include the last character of the range
  (which we exclude by default because of the way we track the point).
  \"Exclusive\" motions do what we normally do, and \"linewise\" motions
  affect all lines they touch.

  Linewise gets difficult: we want to take either a preceeding _or_ a trailing
  newline with the change or deletion _if we have one_."
  (fn [range lines span] span))

(defmethod adjust-for-span :exclusive
  [range _ _]
  range)

(defmethod adjust-for-span :inclusive
  [[start [ei ej]] _ _]
  [start [ei (inc ej)]])

(defmethod adjust-for-span :linewise
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

(defn fix-last-explicit-j
  [[start [ei ej]] {:keys [last-explicit-j]}]
  [start [ei (or ej last-explicit-j)]])

(defn resolve-range
  [{:keys [:avi.document/lines point last-explicit-j] :as edit-context} {:keys [span] :as operation}]
  (when-let [range (resolve/resolve-motion edit-context operation)]
    (-> range
      (fix-last-explicit-j edit-context)
      sort
      (adjust-for-span lines span))))

(defmethod operate :move-point
  [{:keys [last-explicit-j] :as edit-context} operation]
  (+> edit-context
    (let [[i j :as pos] (second (resolve/resolve-motion edit-context operation))
          set-last-explicit? (not (nil? j))
          j (or j last-explicit-j)]
      (if-not pos
        beep/beep)
      (when pos
        (assoc :point [i j])
        (if set-last-explicit?
          (assoc :last-explicit-j j))
        clamp-point-j
        c/adjust-viewport-to-contain-point))))

(defmethod operate :delete
  [{start :point :keys [:avi.document/lines] :as edit-context} operation]
  (+> edit-context
    (if-let [[start end] (resolve-range edit-context operation)]
      (do
        t/start-transaction
        (c/change start end "" :left)
        t/commit
        (operate {:operator :move-point
                  :motion [:goto start]}))
      beep/beep)))
