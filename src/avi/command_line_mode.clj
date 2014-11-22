(ns avi.command-line-mode
  (:require [packthread.core :refer :all]
            [avi.eventmap :as em]
            [avi.buffer :as b]
            [avi.editor :as e]
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

(def eventmap
  (em/eventmap
    ("<Enter>"
      [editor]
      (process-command editor))

    ("<BS>"
      [editor]
      (+> editor
          (let [command-line (:command-line editor)]
            (if (zero? (count command-line))
              (e/enter-mode :normal)
              (assoc :command-line (subs command-line 0 (dec (count command-line))))))))
    
    (:else
      [editor event]
      (+> editor
          (let [[event-type event-data] event]
            (if-not (= event-type :keystroke)
              e/beep
              (append-to-command-line event-data)))))))

(defmethod e/enter-mode :command-line
  [editor mode]
  (assoc editor :mode :command-line, :command-line ""))

(defmethod e/respond :command-line
  [editor event]
  (eventmap editor event))
