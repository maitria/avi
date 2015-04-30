(ns avi.brackets
  (:require [avi.pervasive :refer :all]
            [packthread.core :refer :all]
            [packthread.lenses :as l]
            [avi.buffer :as b]
            [avi.editor :as e]
            [avi.scan :as scan]))

(def ^:private bracket-map
  {\( \)
   \[ \]
   \{ \}})

(def ^:private reverse-bracket-map
  (->> bracket-map
       (map (fn [[a b]] [b a]))
       (into {})))

(def ^:private open-brackets (into #{} (keys bracket-map)))
(def ^:private brackets (into #{} (concat (keys bracket-map) (vals bracket-map))))

(defn- go-to-matching-bracket
  [{[i j] :cursor lines :lines :as lines-and-cursor}]
  (let [bracket (get-in lines [i j])
        open-bracket? (open-brackets bracket)
        scan (if open-bracket?
               (scan/forward [i j] lines)
               (scan/backward [i j] lines))
        brackets (if open-bracket?
                   bracket-map
                   reverse-bracket-map)
        new-cursor (->> scan
                        (reductions
                          (fn [stack [i j]]
                            (let [char (get-in lines [i j])]
                              (cond-> stack
                                (get brackets char) (conj (get brackets char))
                                (= char (first stack)) rest)))
                          ())
                        (drop 1)
                        (map vector scan)
                        (drop-while #(not (empty? (second %))))
                        first
                        first)]
    (when-not (brackets bracket)
      (fail :beep))
    (when-not new-cursor
      (fail :beep))
    (assoc lines-and-cursor :cursor new-cursor)))

(def wrap-go-to-matching-bracket
  (e/keystroke-middleware "%"
    (fn+> [editor]
      (in (l/comp e/current-buffer b/lines-and-cursor)
        go-to-matching-bracket))))
