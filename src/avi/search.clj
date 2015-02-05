(ns avi.search
  (:require [packthread.core :refer :all]
            [avi.command-line :as cl]))

(defn wrap-normal-search-commands
  [responder]
  (fn [editor event]
    (+> editor
      (if (= event [:keystroke "/"])
        (cl/enter :command-line "/")
        (responder event)))))
