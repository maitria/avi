(ns avi.scan
  (:require [avi.buffer.locations :as l]))

(defn line-length
  [lines]
  (fn [i]
    (some-> lines (get i) count)))

(defn forward
  [pos lines]
  (lazy-seq
    (when pos
      (cons pos (forward (l/advance pos (line-length lines)) lines)))))

(defn backward
  [pos lines]
  (lazy-seq
    (when pos
      (cons pos (backward (l/retreat pos (line-length lines)) lines)))))
