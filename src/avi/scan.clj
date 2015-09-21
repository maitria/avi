(ns avi.scan)

(defn advance
  [[i j] line-length]
  (cond
    (>= j (line-length i))
    (if-not (line-length (inc i))
      nil
      [(inc i) 0])

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

(defn line-length
  [lines]
  (fn [i]
    (some-> lines (get i) count)))

(defn forward
  [pos lines]
  (lazy-seq
    (when pos
      (cons pos (forward (advance pos (line-length lines)) lines)))))

(defn backward
  [pos lines]
  (lazy-seq
    (when pos
      (cons pos (backward (retreat pos (line-length lines)) lines)))))
