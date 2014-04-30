(ns avi.command-line-mode
  (:require [packthread.core :refer :all]
            [avi.eventmap :as em]
            [avi.editor :as e]))

(defn- append-to-command-line
  [editor s]
  (assoc editor :command-line (str (:command-line editor) s)))

(defn- line-number?
  [command]
  (every? #(Character/isDigit %) command))

(defn- process-command
  [editor]
  (let [command-line (:command-line editor)]
    (+> editor
        (assoc :mode :normal)
        (cond
          (= "q" command-line)
          (assoc :mode :finished)

          (= "" command-line)
          identity

          (line-number? command-line)
          (e/change-line (constantly (dec (Long/parseLong command-line))))))))

(def eventmap
  (em/eventmap
    ("<Enter>"
      [editor]
      (process-command editor))

    ("<BS>"
      [editor]
      (let [command-line (:command-line editor)]
        (+> editor
            (if (zero? (count command-line))
              (assoc :mode :normal)
              (assoc :command-line (.substring command-line 0 (dec (count command-line))))))))
    
    (:else
      [editor event]
      (let [[event-type event-data] event]
        (+> editor
            (if-not (= event-type :keystroke)
              e/beep
              (append-to-command-line event-data)))))))

(defn enter
  [editor]
  (assoc editor :mode :command-line, :command-line ""))

(defmethod e/respond :command-line
  [editor event]
  (em/invoke-event-handler eventmap editor event))
