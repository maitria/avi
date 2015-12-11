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

(s/defmethod resolve/resolve-motion :word :- (s/maybe l/Location)
  [{:keys [lines] [i j] :point} {n :count}]
  (let [last-location [(dec (count lines)) (dec (count (peek lines)))]
        word-starts (->> (l/forward [i j] (lines/line-length lines))
                      (iterate (fn [stream]
                                 (->> stream
                                   (drop-while (comp word-char? #(get-in lines %)))
                                   (drop-while (complement (comp word-char? #(get-in lines %)))))))
                      (map first)
                      (take-while (complement nil?)))
        locations (concat word-starts [last-location])
        location (nth-or-last locations (or n 1))]
    (if-not (= location [i j])
      location)))
