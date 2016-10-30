(ns avi.edit-context.transactions
  (:require [avi.beep :as beep]
            [avi.document]
            [packthread.core :refer :all]))

(defn start-transaction
  [{:keys [point :avi.document/lines]
    :as edit-context}]
  (when (:avi.document/in-transaction? edit-context)
    (throw (Exception. "attempt to nest a transaction")))
  (+> edit-context
    (update-in [:avi.document/undo-log] conj {:avi.document/lines lines, :point point})
    (assoc :avi.document/in-transaction? true)))

(defn commit
  [edit-context]
  (+> edit-context
      (assoc :avi.document/in-transaction? false
             :avi.document/redo-log ())))
