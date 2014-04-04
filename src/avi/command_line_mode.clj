(ns avi.command-line-mode
  (:require [avi.eventmap :as em]
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
    (cond-> (assoc editor :mode :normal)
      (= "q" command-line)
      (assoc :mode :finished)

      (line-number? command-line)
      (e/change-line (constantly (dec (Long/parseLong command-line)))))))

(def eventmap
  (em/eventmap
    ("<Enter>"
      [editor]
      (process-command editor))
    
    (:else
      [editor event]
      (let [[event-type event-data] event]
        (if-not (= event-type :keystroke)
          (e/beep editor)
          (append-to-command-line editor event-data))))))

(defn enter
  [editor]
  (assoc editor :mode :command-line, :command-line ""))

(defmethod e/process :command-line
  [editor event]
  (em/invoke-event-handler eventmap editor event))
