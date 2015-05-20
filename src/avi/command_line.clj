(ns avi.command-line
  (:require [packthread.core :refer :all]
            [avi.beep :as beep]
            [avi.editor :as e]))

(defn enter
  [editor mode-kw prompt]
  (assoc editor
         :mode mode-kw
         :prompt prompt
         :command-line ""
         ::pre-history (get-in editor [::command-line-history mode-kw])
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

(defn wrap-handle-history-movement
  [responder key from to]
  ((e/keystroke-middleware key
     (fn+> [editor]
       (if-let [command (first (from editor))]
         (do
           (update-in [from] rest)
           (update-in [to] conj (:command-line editor))
           (assoc :command-line command))))) responder))

(defn- command-wrapper
  [command-fn]
  (e/keystroke-middleware "<Enter>"
    (fn+> [editor]
      (let [{:keys [command-line mode]} editor]
        (cond-> (not= "" command-line) (update-in [::command-line-history mode] conj command-line))
        e/enter-normal-mode
        (command-fn (:command-line editor))
        (dissoc :command-line :prompt ::pre-history ::post-history)))))

(defn- responder
  [command-fn]
  (-> beep/beep-responder
      wrap-command-line-insert
      wrap-handle-backspace
      (wrap-handle-history-movement "<C-P>" ::pre-history ::post-history)
      (wrap-handle-history-movement "<C-N>" ::post-history ::pre-history)
      ((command-wrapper command-fn))
      beep/wrap-reset-beep))

(defn mode-middleware
  [mode-kw command-fn]
  (e/mode-middleware mode-kw (responder command-fn)))
