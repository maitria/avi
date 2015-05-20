(ns avi.beep
  (:require [packthread.core :refer :all]))

(defn error
  [editor-like message]
  (+> editor-like
    (assoc :message [:white :red message])))

(defn beep
  [editor-like & [message]]
  (+> editor-like
    (assoc :beep? true)
    (if message
      (error message))))

(defn beep-responder
  [editor event]
  (beep editor))
