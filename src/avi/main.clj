(ns avi.main
  (:require [avi.command-line-mode]
            [avi.insert-mode]
            [avi.normal-mode]
            [avi.editor :as e]))

(def responder
  (-> e/unhandled-event-responder
      avi.normal-mode/wrap-normal-mode
      avi.insert-mode/wrap-insert-mode
      avi.command-line-mode/wrap-command-line-mode
      e/wrap-handle-resize
      e/wrap-handle-exceptions
      e/wrap-reset-beep))
