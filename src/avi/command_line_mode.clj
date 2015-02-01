(ns avi.command-line-mode
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]
            [avi.command-line :as cl]
            [avi.editor :as e]
            [avi.pervasive :refer :all]))

(defn enter-command-line-mode
  [editor prompt]
  (assoc editor
         :mode :command-line
         :prompt prompt
         :command-line ""))

(defn wrap-enter-command-line-mode
  [responder]
  (fn [editor event]
    (+> editor
      (cond
        (= event [:keystroke ":"])
        (enter-command-line-mode ":")

        (= event [:keystroke "/"])
        (enter-command-line-mode "/")

        :else
        (responder event)))))

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
      (e/change-line (constantly (dec (Long/parseLong command-line))))

      :else
      (assoc :message [:white :red (str ":" command-line " is not a thing")]))))

(def wrap-mode (cl/command-line-mode :command-line process-command))
