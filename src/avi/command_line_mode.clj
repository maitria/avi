(ns avi.command-line-mode
  (:require [avi.eventmap :as em]
            [avi.editor :as e]))

(defn- append-to-command-line
  [editor s]
  (assoc editor :command-line (str (:command-line editor) s)))

(def eventmap
  (em/eventmap
    ("<Enter>"
      [editor]
      (assoc editor :mode :finished))
    
    (:else
      [editor event]
      (let [[event-type event-data] event]
        (if-not (= event-type :keystroke)
          (e/beep editor)
          (append-to-command-line editor event-data))))))

(defn enter
  [editor]
  (assoc editor :mode :command-line, :command-line ""))

(defmethod e/process :command-line
  [editor event]
  (em/invoke-event-handler eventmap editor event))
