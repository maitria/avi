(ns avi.buffer.operate.word
  (:require [avi.buffer
              [lines :as lines]
              [locations :as l]]
            [avi.buffer.operate.resolve :as resolve]
            [schema.core :as s]))

(defn word-char?
  [ch]
  (and (char? ch)
       (or (Character/isAlphabetic (int ch))
           (Character/isDigit (int ch))
           (#{\_} ch))))

(defn nth-or-last
  "Return the nth element, or the last if n >= (count coll)."
  [[head & tail] n]
  (if (or (zero? n) (not (seq tail)))
    head
    (recur tail (dec n))))

(defn last-location
  [{:keys [lines]}]
  [(dec (count lines)) (dec (count (peek lines)))])

(defn word-starts
  [{:keys [lines] [i j] :point}]
  (->> (l/forward [i j] (lines/line-length lines))
    (iterate (fn [stream]
               (->> stream
                    (drop-while (comp word-char? #(get-in lines %)))
                    (drop-while (complement (comp word-char? #(get-in lines %)))))))
    (map first)
    (take-while (complement nil?))))

(defn word-locations
  [{:keys [lines] [i j] :point :as buffer}]
  (concat (word-starts buffer) [(last-location buffer)]))

(s/defmethod resolve/resolve-motion :word :- (s/maybe l/Location)
  [{[i j] :point :as buffer} {n :count}]
  (let [location (nth-or-last (word-locations buffer) (or n 1))]
    (if-not (= location [i j])
      location)))
