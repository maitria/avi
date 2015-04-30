(ns avi.brackets
  (:require [avi.pervasive :refer :all]
            [packthread.core :refer :all]
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

(defn- matching-bracket
  [[i j] lines]
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
    (if (brackets bracket)
      new-cursor)))

(def wrap-go-to-matching-bracket
  (e/keystroke-middleware "%"
    (fn+> [editor]
      (let [{[i j] :cursor, lines :lines} (e/current-buffer editor)
            new-cursor (matching-bracket [i j] lines)]
        (if new-cursor
          (in e/current-buffer
            (assoc :cursor new-cursor))
          (fail :beep))))))
