(ns avi.insert-mode
  (:require [avi.editor :as e]
            [avi.buffer :as b]
            [avi.eventmap :as em]))

(defn- insert-key
  [editor s]
  (let [buffer (e/current-buffer editor)
        [i j] (b/cursor buffer)
        current-line (b/line buffer i)
        new-line (str
                   (.substring current-line 0 j)
                   s
                   (.substring current-line j))]
    (e/update-current-buffer editor
                             (fn [buffer]
                               (-> buffer
                                   (assoc-in [:lines i] new-line)
                                   (assoc :cursor [i (inc j)]))))))

(def eventmap
  (em/eventmap
    ("<Esc>"
      [editor]
      editor)

    (:else
      [editor event]
      (let [[event-type event-data] event]
        (if-not (= event-type :keystroke)
          (e/beep editor)
          (insert-key editor event-data))))))

(defmethod e/process :insert
  [editor event]
  (em/invoke-event-handler eventmap editor event))
