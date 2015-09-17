(ns avi.buffer.marks
  (:require [schema.core :as s]))

(defn versioned-mark?
  [mark]
  (= 3 (count mark)))
