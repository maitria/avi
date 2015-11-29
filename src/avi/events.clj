(ns avi.events)

(defn split-string-of-commands
  [key-sequence]
  (lazy-seq
    (if-let [[_ key rest] (re-matches #"^(<[^<]+>|[^<])(.*)$" (or key-sequence ""))]
      (cons key (split-string-of-commands rest)))))

(defn events
  [string-of-commands]
  (->> (split-string-of-commands string-of-commands)
       (map #(vector :keystroke %))
       vec))
