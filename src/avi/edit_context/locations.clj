(ns avi.edit-context.locations
  (:refer-clojure :exclude [replace])
  (:require [schema.core :as s]
            [clojure.spec :as spec]))

(spec/def ::line (complement neg?))
(spec/def ::column (complement neg?))
(spec/def ::location (spec/tuple ::line ::column))
(spec/def ::adjustment-bias #{:left :right})

(def ZLineNumber (s/constrained s/Int (complement neg?)))
(def ColumnNumber (s/constrained s/Int (complement neg?)))

(def Location
  [(s/one ZLineNumber "line number (zero-based)")
   (s/one ColumnNumber "column")])

(def AdjustmentBias
  (s/enum :left :right))

(def boolean? (partial instance? Boolean))

(defn location<
  [a b]
  (< (.compareTo a b) 0))
(spec/fdef location<
  :args (spec/cat :a ::location :b ::location)
  :ret boolean?)

(defn location<=
  [a b]
  (<= (.compareTo a b) 0))
(spec/fdef location<=
  :args (spec/cat :a ::location :b ::location)
  :ret boolean?)

(defn location>
  [a b]
  (> (.compareTo a b) 0))
(spec/fdef location>
  :args (spec/cat :a ::location :b ::location)
  :ret boolean?)

(defn location>=
  [a b]
  (>= (.compareTo a b) 0))
(spec/fdef location>=
  :args (spec/cat :a ::location :b ::location)
  :ret boolean?)

(defn advance
  [[i j] line-length]
  (cond
    (>= j (line-length i))
    (if-not (line-length (inc i))
      nil
      [(inc i) 0])

    :else
    [i (inc j)]))
(spec/fdef advance
  :args (spec/cat :location ::location :line-length (constantly true))
  :ret (spec/nilable ::location))

(defn retreat
  [[i j] line-length]
  (cond
    (= [i j] [0 0])
    nil

    (>= j 1)
    [i (dec j)]

    :else
    [(dec i) (line-length (dec i))]))
(spec/fdef retreat
  :args (spec/cat :location ::location :line-length (constantly true))
  :ret (spec/nilable ::location))

(defn forward
  [pos line-length]
  (lazy-seq
    (when pos
      (cons pos (forward (advance pos line-length) line-length)))))

(defn backward
  [pos line-length]
  (lazy-seq
    (when pos
      (cons pos (backward (retreat pos line-length) line-length)))))

(defn forget-location?
  [a b l]
  (and (location< a l)
       (location< l b)))
(spec/fdef forget-location?
  :args (spec/cat :a ::location :b ::location :l ::location)
  :ret boolean?)

(defn line-count
  [text]
  (reduce (fn [n c]
            (cond-> n
              (= c \newline)
              inc))
          0
          text))

(defn last-line-length
  [text]
  (let [last-newline (.lastIndexOf text (int \newline))]
    (cond-> (count text)
      (not= last-newline -1)
      (- (inc last-newline)))))

(defn adjust-for-replacement
  [[li lj :as l] [ai aj :as a] [bi bj :as b] text bias]
  (cond
    (and (= a b l) (= bias :left))
    l

    (forget-location? a b l)
    nil

    (location<= b l)
    (let [replacement-line-count (line-count text)]
      [(-> li
           (- (- bi ai))
           (+ replacement-line-count))
       (if (= li bi)
         (cond-> (+ (- lj bj) (last-line-length text))
           (zero? replacement-line-count)
           (+ aj)) 
         lj)])

    :else
    l))
(spec/fdef adjust-for-replacement
  :args (spec/cat :l ::location
                  :a ::location
                  :b ::location
                  :text string?
                  :bias ::adjustment-bias)
  :ret (spec/nilable ::location))
