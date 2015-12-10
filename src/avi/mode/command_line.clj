(ns avi.mode.command-line
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]
            [avi.command-line :as cl]
            [avi.editor :as e]
            [avi.pervasive :refer :all]))

(def normal-commands
  {":" (fn+> [editor _]
         (cl/enter :command-line ":"))})

(defn- line-number?
  [command]
  (every? #(Character/isDigit %) command))

(defn- process-command
  [editor command-line]
  (+> editor
    (cond
      (= "q" command-line)
      (assoc :finished? true)

      (= "w" command-line)
      (in e/current-buffer
          (b/write))

      (= "wq" command-line)
      (do
        (in e/current-buffer
            (b/write))
        (assoc :finished? true))

      (= "" command-line)
      identity

      (line-number? command-line)
      (in e/current-buffer
        (b/operate {:operator :move-point
                    :span :linewise
                    :motion [:goto [(dec (Long/parseLong command-line)) :first-non-blank]]}))

      :else
      (assoc :message [:white :red (str ":" command-line " is not a thing")]))))

(def wrap-mode (cl/mode-middleware :command-line process-command))
