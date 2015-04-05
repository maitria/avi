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
  (fn [{:keys [lines] [start-i start-j] :cursor} re]
    (loop [n (inc (count lines))
           i start-i
           j (succ start-j)]
      (if-not (zero? n)
        (if-let [found-j (which (occurrences re (get lines (mod i (count lines))) (partial pred j)))]
          (let [found-pos [(mod i (count lines)) found-j]
                wrapped? (pred (compare found-pos [start-i start-j]) 0)]
            (conj found-pos wrapped?))
          (recur (dec n) (succ i) reset))))))

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
                    (::last-search editor)
                    command-line)]
      (assoc ::last-search pattern)
      (assoc ::last-direction direction)
      (if-let [[i j wrapped?] (find-occurrence direction (e/current-buffer editor) (re-pattern pattern))]
        (do
          (in e/current-buffer (b/move-cursor [i j] j))
          (if wrapped?
            (assoc :message [:red :black "Wrapped to beginning of file!"])))
        (assoc :message [:white :red (str "Did not find `" command-line "`.")])))))

(def wrap-mode
  (comp
    (cl/mode-middleware :forward-search (partial process-search :forward))
    (cl/mode-middleware :backward-search (partial process-search :backward))))

(defn next-occurrence
  [{:keys [::last-direction] :as editor}]
  (process-search last-direction editor ""))

(defn reverse-search-direction
  [editor]
  (update-in editor [::last-direction] #(case % :forward :backward :backward :forward)))

(def next-occurrence-in-opposite-direction
  (comp
    next-occurrence
    reverse-search-direction))

(def wrap-normal-search-commands
  (comp
    (e/keystroke-middleware "n" next-occurrence)
    (e/keystroke-middleware "N" next-occurrence-in-opposite-direction)
    (e/keystroke-middleware "/" #(cl/enter % :forward-search "/"))
    (e/keystroke-middleware "?" #(cl/enter % :backward-search "?"))))

