(ns avi.main
  (:require [avi.command-line-mode]
            [avi.insert-mode]
            [avi.normal-mode]
            [avi.render]
            [avi.editor :as e]))

(defn initial-editor
  [& args]
  (avi.render/rendered (apply e/initial-editor args)))

(def responder
  (-> e/unhandled-event-responder
      avi.normal-mode/wrap-mode
      avi.insert-mode/wrap-mode
      avi.command-line-mode/wrap-mode
      avi.search/wrap-mode
      e/wrap-handle-resize
      e/wrap-handle-exceptions
      e/wrap-reset-beep
      avi.render/wrap))
