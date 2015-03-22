(ns avi.search
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]
            [avi.command-line :as cl]
            [avi.editor :as e]))

(def wrap-normal-search-commands
  (e/keystroke-middleware "/" #(cl/enter % :forward-search "/")))

(defn find-next
  [{:keys [lines], [i] :cursor, :as buffer} re]
  (let [m (re-matcher re (get lines i))]
    (assert (.find m))
    (b/move-cursor buffer [i (.start m)] (.start m))))

(defn process-search
  [editor command-line]
  (+> editor
    (in e/current-buffer
      (find-next (re-pattern command-line)))))

(def wrap-mode (cl/mode-middleware :forward-search process-search))
