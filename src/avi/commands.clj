(ns avi.commands
  "Avi's command-line-mode commands.
  
  Functions implemented in this namespace can be called by name from the colon
  prompt."
  (:require [avi.edit-context :as b]
            [avi.editor :as e]
            [packthread.core :refer :all]))

(defn -NUMBER-
  "Special function which handles commands like `:42`"
  [editor command-line]
  (+> editor
    (in e/edit-context
      (b/operate {:operator :move-point
                  :motion [:goto [(dec (Long/parseLong command-line)) :first-non-blank]]}))))

(defn q
  [editor]
  (assoc editor :finished? true))

(defn w
  [editor]
  (+> editor
    (in e/edit-context
      b/write)))

(def wq (comp q w))
