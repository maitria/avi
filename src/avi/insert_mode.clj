(ns avi.insert-mode
  (:require [packthread.core :refer :all]
            [avi.editor :as e]
            [avi.eventmap :as em]
            [avi.buffer :as b]
            [avi.pervasive :refer :all]))

(defn enter-insert-mode
  [editor & {script :script-prefix
             :or {script []}}]
  (+> editor
      (assoc :old-mode :insert,
             :message [:white :black "--INSERT--"]
             :insert-mode-state {:count (or (:count editor) 1)
                                 :script script})
      (in e/current-buffer
          b/start-transaction)))

(def wrap-enter-insert-mode
  (em/eventmap
    ("a"
      [editor repeat-count]
      (+> editor
          (enter-insert-mode)
          (in e/current-buffer
            (let [{[i j] :cursor, lines :lines} (e/current-buffer editor)
                  new-j (min (count (get lines i)) (inc j))]
              (assoc :cursor [i new-j])))))

    ("i"
      [editor repeat-count]
      (enter-insert-mode editor))

    ("o"
      [editor repeat-count]
      (+> editor
          (let [{[i] :cursor} (e/current-buffer editor)]
            (enter-insert-mode :script-prefix [[:keystroke "<Enter>"]])
            (in e/current-buffer
                (b/insert-blank-line (inc i)))
            (e/change-line inc))))

    ("A"
      [editor repeat-count]
      (+> editor
          (enter-insert-mode)
          (in e/current-buffer
            (let [{[i] :cursor, lines :lines} (e/current-buffer editor)
                  j (count (get lines i))]
              (assoc :cursor [i j])))))

    ("O"
      [editor repeat-count]
      (+> editor
          (let [{[i] :cursor} (e/current-buffer editor)]
            (enter-insert-mode :script-prefix [[:keystroke "<Enter>"]])
            (in e/current-buffer
                (b/insert-blank-line i)
                (b/move-cursor [i 0] 0)))))))

(defn- key->text
  [key]
  (if (= key "<Enter>")
    "\n"
    key))

(defn- update-buffer-for-insert-event
  [editor [event-type event-data :as event]]
  (when-not (= event-type :keystroke)
    (fail :beep))
  (+> editor
    (in e/current-buffer
        (if (= event-data "<BS>")
          (if (= [0 0] (:cursor (e/current-buffer editor)))
            (fail :beep)
            b/backspace)
          (b/insert-text (key->text event-data))))))

(defn- play-script
  [editor script]
  (reduce
    update-buffer-for-insert-event
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

(defn- wrap-handle-escape
  [responder]
  (fn [editor event]
    (if (= event [:keystroke "<Esc>"])
      (+> editor
          play-script-repeat-count-times
          (dissoc :insert-mode-state)
          (let [b (e/current-buffer editor)
                [i j] (:cursor b)
                new-j (max (dec j) 0)]
            (in e/current-buffer
                (b/move-cursor [i new-j] new-j)
                b/commit))
          (e/enter-normal-mode))
      (responder editor event))))

(defn- wrap-record-event
  [responder]
  (fn [editor event]
    (+> editor
        (responder event)
        (update-in [:insert-mode-state :script] conj event))))

(def responder
  (-> update-buffer-for-insert-event
      wrap-record-event
      wrap-handle-escape
      em/wrap-reset-beep))

(defmethod e/respond :insert
  [editor event]
  (responder editor event))
