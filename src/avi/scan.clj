(ns avi.scan)

(defn advance
  [[i j] lines]
  (cond
    (>= i (count lines))
    nil

    (>= j (count (get lines i)))
    (recur [(inc i) 0] lines)

    :else
    [i (inc j)]))

(defn retreat
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

(defn forward
  [pos lines]
  (lazy-seq
    (if-let [[i j] pos]
      (cons
        [i j]
        (forward (advance pos lines) lines))
      nil)))

(defn backward
  [pos lines]
  (lazy-seq
    (if-let [[i j] pos]
      (cons
        [i j]
        (backward (retreat pos lines) lines))
    nil)))
