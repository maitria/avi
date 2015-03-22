(ns avi.search
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]
            [avi.command-line :as cl]
            [avi.editor :as e]
            [avi.pervasive :refer :all]))

(def wrap-normal-search-commands
  (e/keystroke-middleware "/" #(cl/enter % :forward-search "/")))

(defn find-next
  [lines [i j] re]
  (if (>= i (count lines))
    nil
    (let [m (re-matcher re (get lines i))]
      (if (.find m)
        [i (.start m)]
        (recur lines [(inc i) 0] re)))))

(defn process-search
  [editor command-line]
  (+> editor
    (let [{:keys [lines] [i j] :cursor} (e/current-buffer editor)
          p (find-next lines [i j] (re-pattern command-line))]
      (if p
        (in e/current-buffer (b/move-cursor p (second p)))
        (assoc :message [:white :red (str "Did not find `" command-line "`.")])))))

(def wrap-mode (cl/mode-middleware :forward-search process-search))
