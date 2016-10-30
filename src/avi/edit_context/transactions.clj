(ns avi.edit-context.transactions
  (:require [avi.beep :as beep]
            [avi.documents]
            [packthread.core :refer :all]))

(defn start-transaction
  [{:keys [:avi.lenses/point :avi.documents/lines]
    :as edit-context}]
  (when (:avi.documents/in-transaction? edit-context)
    (throw (Exception. "attempt to nest a transaction")))
  (+> edit-context
    (update-in [:avi.documents/undo-log] conj {:avi.documents/lines lines, :avi.lenses/point point})
    (assoc :avi.documents/in-transaction? true)))

(defn commit
  [edit-context]
  (+> edit-context
      (assoc :avi.documents/in-transaction? false
             :avi.documents/redo-log ())))
