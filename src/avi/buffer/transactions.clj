(ns avi.buffer.transactions
  (:require [avi.beep :as beep]
            [packthread.core :refer :all]))

(defn start-transaction
  [{lines :lines,
    point :point,
    :as buffer}]
  (when (:in-transaction? buffer)
    (throw (Exception. "attempt to nest a transaction")))
  (+> buffer
    (update-in [:undo-log] conj {:lines lines, :point point})
    (assoc :in-transaction? true)))

(defn commit
  [buffer]
  (+> buffer
      (assoc :in-transaction? false
             :redo-log ())))
