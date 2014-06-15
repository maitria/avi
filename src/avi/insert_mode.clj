(ns avi.insert-mode
  (:require [packthread.core :refer :all]
            [avi.editor :as e]
            [avi.buffer :as b]
            [avi.eventmap :as em]))

(defn- record-event
  [editor event]
  (update-in editor [:insert-mode-state :script] conj event))

(defn- key->text
  [key]
  (if (= key "<Enter>")
    "\n"
    key))

(defn- insert-key
  [editor [_ event-data :as event]]
  (+> editor
    (record-event event)
    (in e/current-buffer
        (if (= event-data "<BS>")
          b/backspace
          (b/insert-text (key->text event-data))))))

(defn- play-script
  [editor script]
  (reduce
    e/respond
    editor
    script))

(defn- play-script-repeat-count-times
  [editor]
  (let [{script :script,
         repeat-count :count} (:insert-mode-state editor)]
    (reduce
      (fn [editor n]
        (play-script editor script))
      editor
      (range (dec repeat-count)))))

(def eventmap
  (em/eventmap
    ("<Esc>"
      [editor]
      (+> editor
          play-script-repeat-count-times
          (dissoc :insert-mode-state)
          (let [b (e/current-buffer editor)
                [i j] (:cursor b)
                new-j (max (dec j) 0)]
            (in e/current-buffer
                (b/move-cursor [i new-j] new-j)))
          (e/enter-mode :normal)))

    ("<BS>"
      [editor event]
      (+> editor
          (let [[i j] (:cursor (e/current-buffer editor))]
            (if (= [0 0] [i j])
              e/beep
              (insert-key event)))))

    (:else
      [editor event]
      (+> editor
          (let [[event-type event-data] event]
            (if-not (= event-type :keystroke)
              e/beep
              (insert-key event)))))))

(defmethod e/respond :insert
  [editor event]
  (em/invoke-event-handler eventmap editor event))

(defmethod e/enter-mode :insert
  [editor mode]
  (+> editor
      (assoc :mode :insert,
             :message [:white :black "--INSERT--"]
             :insert-mode-state {:count (or (:count editor) 1)
                                 :script []})))
