(ns avi.search
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]
            [avi.command-line :as cl]
            [avi.editor :as e]
            [avi.pervasive :refer :all]))

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

(def find-forward (scanner inc first <= 0))
(def find-backward  (scanner dec last >= Long/MAX_VALUE))

(defn find-occurrence
  [direction & args]
  (let [scanner (case direction
                  :forward find-forward
                  :backward find-backward)]
    (apply scanner args)))

(defn process-search
  [direction editor command-line]
  (+> editor
    (let [pattern (if (= "" command-line)
                    (get-in editor [::last-search direction])
                    command-line)]
      (assoc-in [::last-search direction] pattern)
      (assoc ::last-direction direction)
      (if-let [[i j] (find-occurrence direction (e/current-buffer editor) (re-pattern pattern))]
        (in e/current-buffer (b/move-cursor [i j] j))
        (assoc :message [:white :red (str "Did not find `" command-line "`.")])))))

(def wrap-mode
  (comp
    (cl/mode-middleware :forward-search (partial process-search :forward))
    (cl/mode-middleware :backward-search (partial process-search :backward))))

(defn next-occurrence
  [{:keys [::last-direction] :as editor}]
  (process-search last-direction editor ""))

(def wrap-normal-search-commands
  (comp
    (e/keystroke-middleware "n" next-occurrence)
    (e/keystroke-middleware "/" #(cl/enter % :forward-search "/"))
    (e/keystroke-middleware "?" #(cl/enter % :backward-search "?"))))

