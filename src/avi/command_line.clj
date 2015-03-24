(ns avi.command-line
  (:require [packthread.core :refer :all]
            [avi.editor :as e]))

(defn enter
  [editor mode-kw prompt]
  (assoc editor
         :mode mode-kw
         :prompt prompt
         :command-line ""
         ::pre-history (get-in editor [:command-line-history mode-kw])
         ::post-history '()))

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

(def wrap-handle-previous-command
  (e/keystroke-middleware "<C-P>"
    (fn+> [editor]
      (if-let [command (first (::pre-history editor))]
        (do
          (update-in [::pre-history] rest)
          (update-in [::post-history] conj (:command-line editor))
          (assoc :command-line command))))))

(def wrap-handle-next-command
  (e/keystroke-middleware "<C-N>"
    (fn+> [editor]
      (if-let [command (first (::post-history editor))]
        (do
          (update-in [::post-history] rest)
          (update-in [::pre-history] conj (:command-line editor))
          (assoc :command-line command))))))

(defn- command-wrapper
  [command-fn]
  (e/keystroke-middleware "<Enter>"
    (fn+> [editor]
      (update-in [:command-line-history (:mode editor)] conj (:command-line editor))
      e/enter-normal-mode
      (command-fn (:command-line editor))
      (dissoc :command-line :prompt ::pre-history ::post-history))))

(defn- responder
  [command-fn]
  (-> e/beep-responder
      wrap-command-line-insert
      wrap-handle-backspace
      wrap-handle-previous-command
      wrap-handle-next-command
      ((command-wrapper command-fn))
      e/wrap-reset-beep))

(defn mode-middleware
  [mode-kw command-fn]
  (e/mode-middleware mode-kw (responder command-fn)))