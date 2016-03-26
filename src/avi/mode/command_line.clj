(ns avi.mode.command-line
  (:require [packthread.core :refer :all]
            [avi.edit-context :as b]
            [avi.command-line :as cl]
            [avi.commands]
            [avi.editor :as e]
            [avi.pervasive :refer :all]))

(def normal-commands
  {":" (fn+> [editor _]
         (cl/enter :command-line ":"))})

(defn- line-number?
  [command]
  (every? #(Character/isDigit %) command))

(defn- command-fn
  [command-line]
  (ns-resolve 'avi.commands (symbol command-line)))

(defn- process-command
  [editor command-line]
  (+> editor
    (cond
      (= "" command-line)
      identity

      (line-number? command-line)
      (avi.commands/-NUMBER- command-line)

      (command-fn command-line)
      ((command-fn command-line))

      :else
      (assoc :message [:white :red (str ":" command-line " is not a thing")]))))

(def wrap-mode (cl/mode-middleware :command-line process-command))
