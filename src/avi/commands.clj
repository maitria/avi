(ns avi.commands
  (:require [avi.buffer :as b]
            [avi.editor :as e]
            [packthread.core :refer :all]))

(defn -NUMBER-
  "Special function which handles commands like `:42`"
  [editor command-line]
  (+> editor
    (in e/current-buffer
      (b/operate {:operator :move-point
                  :motion [:goto [(dec (Long/parseLong command-line)) :first-non-blank]]}))))

(defn q
  [editor]
  (assoc editor :finished? true))

(defn w
  [editor]
  (+> editor
    (in e/current-buffer
      b/write)))

(def wq (comp q w))
