(ns avi.scan)

(defn advance
  [[i j] lines]
  (cond
    (>= i (count lines))
    nil

    (>= j (count (get lines i)))
    [(inc i) 0]

    :else
    [i (inc j)]))

(defn retreat
  [[i j] lines]
  (cond
    (= [i j] [0 0])
    nil

    (>= j 1)
    [i (dec j)]

    :else
    [(dec i) (count (get lines (dec i)))]))

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
