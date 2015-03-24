(ns avi.search
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]
            [avi.command-line :as cl]
            [avi.editor :as e]
            [avi.pervasive :refer :all]))

(def wrap-normal-search-commands
  (comp
    (e/keystroke-middleware "/" #(cl/enter % :forward-search "/"))
    (e/keystroke-middleware "?" #(cl/enter % :backward-search "?"))))

(defn occurrences
  [re s pred]
  (let [m (re-matcher re s)]
    (loop [ret []]
      (cond
        (not (.find m))
        ret

        (not (pred (.start m)))
        (recur ret)

        :else
        (recur (conj ret (.start m)))))))

(defn scanner
  [succ which pred reset]
  (fn f
    ([{:keys [lines] [i j] :cursor} re]
     (f lines [i (succ j)] re))
    ([lines [i j] re]
     (if-not (contains? lines i)
       nil
       (if-let [found-j (which (occurrences re (get lines i) (partial pred j)))]
         [i found-j]
         (recur lines [(succ i) reset] re))))))

(def next-occurrence-position (scanner inc first <= 0))
(def previous-occurrence-position (scanner dec last >= Long/MAX_VALUE))

(defn process-search
  [scanner editor command-line]
  (+> editor
    (if-let [[i j] (scanner (e/current-buffer editor) (re-pattern command-line))]
       (in e/current-buffer (b/move-cursor [i j] j))
       (assoc :message [:white :red (str "Did not find `" command-line "`.")]))))

(def wrap-mode
  (comp
    (cl/mode-middleware :forward-search (partial process-search next-occurrence-position))
    (cl/mode-middleware :backward-search (partial process-search previous-occurrence-position))))
