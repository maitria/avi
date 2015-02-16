(ns avi.search
  (:require [packthread.core :refer :all]
            [avi.command-line :as cl]
            [avi.editor :as e]))

(def wrap-normal-search-commands
  (e/keystroke-middleware "/" #(cl/enter % :forward-search "/")))

(defn process-search
  [editor command-line]
  editor)

(def wrap-mode (cl/mode-middleware :forward-search process-search))
