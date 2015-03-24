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

(defn previous-occurrence-position
  ([{:keys [lines] [i j] :cursor} re]
   (previous-occurrence-position lines [i (dec j)] re))
  ([lines [i j] re]
   (if (< i 0)
     nil
     (if-let [found-j (last (occurrences re (get lines i) #(>= j %)))]
       [i found-j]
       (recur lines [(dec i) Long/MAX_VALUE] re)))))

(defn process-search
  [finder editor command-line]
  (+> editor
    (if-let [[i j] (finder (e/current-buffer editor) (re-pattern command-line))]
       (in e/current-buffer (b/move-cursor [i j] j))
       (assoc :message [:white :red (str "Did not find `" command-line "`.")]))))

(def wrap-mode
  (comp
    (cl/mode-middleware :forward-search (partial process-search next-occurrence-position))
    (cl/mode-middleware :backward-search (partial process-search previous-occurrence-position))))
