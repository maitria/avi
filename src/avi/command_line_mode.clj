(ns avi.command-line-mode
  (:require [avi.eventmap :as em]))

(def eventmap
  (em/eventmap
    ("<Enter>"
      [editor]
      (assoc editor :mode :finished))

    ("a"
      [editor]
      (assoc editor :command-line (str (:command-line editor) "a")))

    ("b"
      [editor]
      (assoc editor :command-line (str (:command-line editor) "b")))

    ("c"
      [editor]
      (assoc editor :command-line (str (:command-line editor) "c")))))

(defn process
  [editor event]
  (em/invoke-event-handler eventmap editor event))
