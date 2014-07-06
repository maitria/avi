(ns avi.brackets)

(defn- advance-position
  [[i j] lines]
  (cond
    (>= i (count lines))
    nil
    
    (>= j (count (get lines i)))
    (recur [(inc i) 0] lines)

    :else
    [i (inc j)]))

(defn- retreat-position
  [[i j] lines]
  (cond
    (< i 0)
    nil

    (>= j 1)
    [i (dec j)]

    :else
    (recur
      [(dec i) (count (get lines (dec i)))]
      lines)))

(defn- forward-scan
  [pos lines]
  (lazy-seq
    (if-let [[i j] pos]
      (cons
        [i j]
        (forward-scan (advance-position pos lines) lines))
      nil)))

(defn- backward-scan
  [pos lines]
  (lazy-seq
    (if-let [[i j] pos]
      (cons
        [i j]
        (backward-scan (retreat-position pos lines) lines))
    nil)))

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

(defn matching-bracket
  [[i j] lines]
  (let [bracket (get-in lines [i j])
        open-bracket? (open-brackets bracket)
        scan (if open-bracket?
               (forward-scan [i j] lines)
               (backward-scan [i j] lines))
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
