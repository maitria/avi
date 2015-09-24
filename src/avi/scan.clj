(ns avi.scan
  (:require [avi.buffer
             [lines :as lines]
             [locations :as l]]))

(defn forward
  [pos lines]
  (lazy-seq
    (when pos
      (cons pos (forward (l/advance pos (lines/line-length lines)) lines)))))

(defn backward
  [pos lines]
  (lazy-seq
    (when pos
      (cons pos (backward (l/retreat pos (lines/line-length lines)) lines)))))
