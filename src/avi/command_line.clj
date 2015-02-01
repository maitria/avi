(ns avi.command-line
  (:require [packthread.core :refer :all]
            [avi.editor :as e]))

(defn- append-to-command-line
  [editor s]
  (assoc editor :command-line (str (:command-line editor) s)))

(defn- wrap-command-line-insert
  [responder]
  (fn [editor [event-type event-data :as event]]
    (+> editor
      (if (= event-type :keystroke)
        (append-to-command-line event-data)
        (responder event)))))

(defn- wrap-handle-backspace
  [responder]
  (fn [editor event]
    (+> editor
      (if (= event [:keystroke "<BS>"])
        (let [command-line (:command-line editor)]
          (if (zero? (count command-line))
            (e/enter-normal-mode)
            (assoc :command-line (subs command-line 0 (dec (count command-line))))))
        (responder event)))))

(defn- command-wrapper
  [command-fn]
  (fn [responder]
    (fn [editor event]
      (+> editor
        (if (= event [:keystroke "<Enter>"])
          (do
            (e/enter-normal-mode)
            (command-fn (:command-line editor)))
          (responder event))))))

(defn- responder
  [command-fn]
  (-> e/beep-responder
      wrap-command-line-insert
      wrap-handle-backspace
      ((command-wrapper command-fn))
      e/wrap-reset-beep))

(defn command-line-mode
  [mode-kw command-fn]
  (e/mode-middleware mode-kw (responder command-fn)))
