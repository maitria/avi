(ns avi.insert-mode
  (:require [packthread.core :refer :all]
            [avi.editor :as e]
            [avi.buffer :as b]
            [avi.eventmap :as em]
            [avi.pervasive :refer :all]))

(def eventmap
  (em/eventmap
    ("<Esc>"
      [editor]
      (+> editor
          (let [b (e/current-buffer editor)
                [i j] (b/cursor b)
                new-j (max (dec j) 0)]
            (in e/current-buffer
                (b/move-cursor [i new-j] new-j)))
          (assoc :mode :normal)))

    ("<BS>"
      [editor]
      (+> editor
          (let [[i j] (b/cursor (e/current-buffer editor))]
            (cond
              (= [0 0] [i j])
              e/beep

              (= 0 j)
              (in e/current-buffer
                (let [{lines :lines,
                       :as buffer} (e/current-buffer editor)
                      new-line (str (get lines (dec i)) (get lines i))
                      new-lines (splice lines (dec i) (inc i) [new-line])]
                  (assoc :lines new-lines)))

              :else
              (in e/current-buffer
                  (b/backspace))))))

    ("<Enter>"
      [editor]
      (+> editor
          (in e/current-buffer
              (b/insert-text "\n"))))

    (:else
      [editor event]
      (+> editor
          (let [[event-type event-data] event]
            (if-not (= event-type :keystroke)
              e/beep
              (in e/current-buffer
                  (b/insert-text event-data))))))))

(defmethod e/respond :insert
  [editor event]
  (em/invoke-event-handler eventmap editor event))
