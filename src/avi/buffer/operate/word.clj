(ns avi.buffer.operate.word
  (:require [avi.buffer
              [lines :as lines]
              [locations :as l]]
            [avi.buffer.operate.resolve :as resolve]
            [avi.nfa :as nfa]
            [schema.core :as s]))

(defn word-char?
  [ch]
  (and (char? ch)
       (or (Character/isAlphabetic (int ch))
           (Character/isDigit (int ch))
           (#{\_} ch))))

(defn classify
  [ch]
  (cond
    (or (nil? ch) (Character/isWhitespace ch)) :ws
    (word-char? ch)                            :word
    :else                                      :other))

(def ws (nfa/match :ws))
(def ws+ (nfa/chain ws (nfa/kleene ws)))
(def word (nfa/match :word))
(def word+ (nfa/chain word (nfa/kleene word)))
(def other (nfa/match :other))
(def other+ (nfa/chain other (nfa/kleene other)))

(def first-of-next-word-nfa
  (nfa/choice
    (nfa/chain ws+ (nfa/choice word other))
    (nfa/chain other+ (nfa/choice
                        (nfa/chain (nfa/kleene ws) word)
                        (nfa/chain ws+ other)))
    (nfa/chain word+ (nfa/choice
                       (nfa/chain (nfa/kleene ws) other)
                       (nfa/chain ws+ word)))))

(defn last-location
  [{:keys [lines]}]
  [(dec (count lines)) (dec (count (peek lines)))])

(s/defmethod resolve/resolve-motion :word :- (s/maybe l/Location)
  [{:keys [lines point] :as buffer} {n :count}]
  (loop [[i j] point
         n (or n 1)]
    (if (zero? n)
      (if-not (= point [i j])
        [i j])
      (recur
        (loop [[[i j] :as stream] (l/forward [i j] (lines/line-length lines))
               state (nfa/start first-of-next-word-nfa)]
          (if-not stream
            (last-location buffer)
            (let [state' (nfa/advance first-of-next-word-nfa state (classify (get-in lines [i j])) :reject)]
              (assert (not= state' :reject))
              (if (nfa/accept? first-of-next-word-nfa state')
                [i j]
                (recur (next stream) state')))))
        (dec n)))))
