(ns avi.commands
  (:require [avi.buffer :as b]
            [avi.editor :as e]
            [packthread.core :refer :all]))

(defn q
  [editor]
  (assoc editor :finished? true))

(defn w
  [editor]
  (+> editor
    (in e/current-buffer
      b/write)))

(def wq (comp q w))
