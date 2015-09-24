(ns avi.scan
  (:require [avi.buffer
             [locations :as l]]))

(defn forward
  [pos line-length]
  (lazy-seq
    (when pos
      (cons pos (forward (l/advance pos line-length) line-length)))))

(defn backward
  [pos line-length]
  (lazy-seq
    (when pos
      (cons pos (backward (l/retreat pos line-length) line-length)))))
