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
      (if-not (= (first event) :keystroke)
        (e/beep editor)
        (append-to-command-line editor (second event))))))

(defn process
  [editor event]
  (em/invoke-event-handler eventmap editor event))
