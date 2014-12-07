(ns avi.command-line-mode
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]
            [avi.editor :as e]
            [avi.eventmap :as em]
            [avi.pervasive :refer :all]))

(defn- append-to-command-line
  [editor s]
  (assoc editor :command-line (str (:command-line editor) s)))

(defn- line-number?
  [command]
  (every? #(Character/isDigit %) command))

(defn- process-command
  [editor]
  (+> editor
      (let [command-line (:command-line editor)]
        (e/enter-mode :normal)
        (cond
          (= "q" command-line)
          (e/enter-mode :finished)

          (= "w" command-line)
          (in e/current-buffer
              (b/write))

          (= "wq" command-line)
          (do
            (in e/current-buffer
                (b/write))
            (e/enter-mode :finished))

          (= "" command-line)
          identity

          (line-number? command-line)
          (e/change-line (constantly (dec (Long/parseLong command-line))))
          
          :else
          (assoc :message [:white :red (str ":" command-line " is not a thing")])))))

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
            (e/enter-mode :normal)
            (assoc :command-line (subs command-line 0 (dec (count command-line))))))
        (responder event)))))

(defn- wrap-process-command
  [responder]
  (fn [editor event]
    (if (= event [:keystroke "<Enter>"])
      (process-command editor)
      (responder editor event))))

(def responder
  (-> em/beep-responder
      wrap-command-line-insert
      wrap-handle-backspace
      wrap-process-command
      em/wrap-reset-beep))

(defmethod e/enter-mode :command-line
  [editor mode]
  (assoc editor :mode :command-line, :command-line ""))

(defmethod e/respond :command-line
  [editor event]
  (responder editor event))
