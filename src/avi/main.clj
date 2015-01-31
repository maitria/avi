(ns avi.main
  (:require [avi.command-line-mode]
            [avi.insert-mode]
            [avi.normal-mode]
            [avi.editor :as e]
            [avi.eventmap :as em]))

require
(def responder
  (-> avi.normal-mode/responder
      avi.insert-mode/wrap-insert-mode
      avi.command-line-mode/wrap-command-line-mode
      e/wrap-handle-resize
      e/wrap-handle-exceptions
      em/wrap-reset-beep))
