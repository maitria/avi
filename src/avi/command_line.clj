(ns avi.command-line
  (:require [packthread.core :refer :all]
            [avi.editor :as e]))

(defn enter
  [editor mode-kw prompt]
  (assoc editor
         :mode mode-kw
         :prompt prompt
         :command-line ""))

(defn- append-to-command-line
  [editor s]
  (assoc editor :command-line (str (:command-line editor) s)))

(defn- wrap-command-line-insert
  [responder]
  (fn+> [editor [event-type event-data :as event]]
    (if (= event-type :keystroke)
      (append-to-command-line event-data)
      (responder event))))

(def wrap-handle-backspace
  (e/keystroke-middleware "<BS>"
    (fn+> [editor]
      (let [command-line (:command-line editor)]
        (if (zero? (count command-line))
          (e/enter-normal-mode)
          (assoc :command-line (subs command-line 0 (dec (count command-line)))))))))

(defn- command-wrapper
  [command-fn]
  (e/keystroke-middleware "<Enter>"
    (fn+> [editor]
      e/enter-normal-mode
      (command-fn (:command-line editor)))))

(defn- responder
  [command-fn]
  (-> e/beep-responder
      wrap-command-line-insert
      wrap-handle-backspace
      ((command-wrapper command-fn))
      e/wrap-reset-beep))

(defn mode-middleware
  [mode-kw command-fn]
  (e/mode-middleware mode-kw (responder command-fn)))
