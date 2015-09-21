(ns avi.scan)

(defn advance
  [[i j] lines]
  (cond
    (= [i j] [(dec (count lines)) (dec (count (last lines)))])
    nil

    (>= j (count (get lines i)))
    [(inc i) 0]

    :else
    [i (inc j)]))

(defn retreat
  [[i j] line-length]
  (cond
    (= [i j] [0 0])
    nil

    (>= j 1)
    [i (dec j)]

    :else
    [(dec i) (line-length (dec i))]))

(defn forward
  [pos lines]
  (lazy-seq
    (when pos
      (cons pos (forward (advance pos lines) lines)))))

(defn backward
  [pos lines]
  (lazy-seq
    (when pos
      (cons pos (backward (retreat pos #(count (get lines %))) lines)))))
