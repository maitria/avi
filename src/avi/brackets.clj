(ns avi.brackets)

(defn- advance-position
  [[i j] lines]
  (if (>= (inc j) (count (get lines i)))
    (if (= (inc i) (count lines))
      nil
      [(inc i) 0])
    [i (inc j)]))

(defn- retreat-position
  [[i j] lines]
  (if (zero? j)
    (if (zero? i)
      nil
      [(dec i) (dec (count (get lines (dec i))))])
    [i (dec j)]))

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

(defn matching-bracket
  [[i j] lines]
  (let [bracket (get-in lines [i j])
        open-bracket? (#{\( \[ \{ \<} bracket)
        scan (if open-bracket?
               (forward-scan [i j] lines)
               (backward-scan [i j] lines))
        brackets (if open-bracket?
                   {\( \) \[ \] \{ \} \< \>}
                   {\) \( \] \[ \} \{ \> \<})
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
    new-cursor))