(ns avi.buffer.transactions
  (:require [avi.beep :as beep]
            [packthread.core :refer :all]))

(defn start-transaction
  [{lines :lines,
    point :point,
    :as edit-context}]
  (when (:in-transaction? edit-context)
    (throw (Exception. "attempt to nest a transaction")))
  (+> edit-context
    (update-in [:undo-log] conj {:lines lines, :point point})
    (assoc :in-transaction? true)))

(defn commit
  [edit-context]
  (+> edit-context
      (assoc :in-transaction? false
             :redo-log ())))
