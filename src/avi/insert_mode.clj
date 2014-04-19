(ns avi.insert-mode
  (:require [avi.editor :as e]
            [avi.buffer :as b]
            [avi.eventmap :as em]))

(def eventmap
  (em/eventmap
    ("<Esc>"
      [editor]
      (assoc editor :mode :normal))

    (:else
      [editor event]
      (let [[event-type event-data] event]
        (if-not (= event-type :keystroke)
          (e/beep editor)
          (e/update-current-buffer editor #(b/insert % event-data)))))))

(defmethod e/respond :insert
  [editor event]
  (em/invoke-event-handler eventmap editor event))
