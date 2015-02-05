(ns avi.search
  (:require [packthread.core :refer :all]
            [avi.command-line :as cl]
            [avi.editor :as e]))

(def wrap-normal-search-commands
  (e/keystroke-middleware "/" #(cl/enter % :command-line "/")))
