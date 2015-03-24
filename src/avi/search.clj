(ns avi.search
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]
            [avi.command-line :as cl]
            [avi.editor :as e]
            [avi.pervasive :refer :all]))

(def wrap-normal-search-commands
  (e/keystroke-middleware "/" #(cl/enter % :forward-search "/")))

(defn next-occurrence-position
  ([{:keys [lines] [i j] :cursor} re]
   (next-occurrence-position lines [i (inc j)] re))
  ([lines [i j] re]
   (if (>= i (count lines))
     nil
     (let [m (re-matcher re (get lines i))]
       (if (.find m j)
         [i (.start m)]
         (recur lines [(inc i) 0] re))))))

(defn process-search
  [editor command-line]
  (+> editor
    (if-let [[i j] (next-occurrence-position (e/current-buffer editor) (re-pattern command-line))]
       (in e/current-buffer (b/move-cursor [i j] j))
       (assoc :message [:white :red (str "Did not find `" command-line "`.")]))))

(def wrap-mode (cl/mode-middleware :forward-search process-search))
