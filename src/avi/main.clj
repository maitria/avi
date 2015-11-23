(ns avi.main
  (:require [avi.beep :as beep]
            [avi.mode command-line insert normal]
            [avi.render]
            [avi.editor :as e]))

(defn initial-editor
  [& args]
  (avi.render/rendered (apply e/initial-editor args)))

(defn responder
  [catch-errors?]
  (-> beep/unhandled-event-responder
    avi.mode.normal/wrap-mode
    avi.mode.insert/wrap-mode
    avi.mode.command-line/wrap-mode
    avi.search/wrap-mode
    e/wrap-handle-resize
    (cond-> catch-errors? e/wrap-handle-exceptions)
    beep/wrap-reset-beep
    avi.render/wrap))