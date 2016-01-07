(ns avi.buffer.operate.word
  (:require [avi.buffer
              [lines :as lines]
              [locations :as l]]
            [avi.buffer.operate.resolve :as resolve]
            [avi.nfa :as nfa]
            [avi.pervasive :refer :all]
            [schema.core :as s]))

(defn word-char?
  [ch]
  (and (char? ch)
       (or (Character/isAlphabetic (int ch))
           (Character/isDigit (int ch))
           (#{\_} ch))))

(defn classify
  [ch big?]
  (cond
    (nil? ch)                   :nl
    (Character/isWhitespace ch) :ws
    (word-char? ch)             (if big? :other :word)
    :else                       :other))

(def nl (nfa/match :nl))
(def ws (nfa/choice (nfa/match :ws) nl))
(def ws+ (nfa/chain ws (nfa/kleene ws)))
(def word (nfa/match :word))
(def word+ (nfa/chain word (nfa/kleene word)))
(def other (nfa/match :other))
(def other+ (nfa/chain other (nfa/kleene other)))

(def first-of-next-word-nfa
  (nfa/choice
    (nfa/chain (nfa/kleene nfa/any) nl nl)
    (nfa/chain ws+ (nfa/choice word other))
    (nfa/chain other+ (nfa/choice
                        (nfa/chain (nfa/kleene ws) word)
                        (nfa/chain ws+ other)))
    (nfa/chain word+ (nfa/choice
                       (nfa/chain (nfa/kleene ws) other)
                       (nfa/chain ws+ word)))))

(defn last-possible
  [{:keys [lines]} {:keys [operator] [_ {:keys [direction]}] :motion}]
  (if (= direction :backward)
    [0 0]
    (let [i (dec (count lines))
          j (cond-> (count (peek lines))
              (= operator :move-point)
              dec)]
      [i j])))

(defn next-word
  [{:keys [lines] :as buffer} {[_ {:keys [big? direction]}] :motion, :as operation} [i j]]
  (loop [[[i j] :as stream] ((if (= direction :backward)
                               l/backward
                               l/forward) [i j] (lines/line-length lines))
         state (nfa/start first-of-next-word-nfa)]
    (if-not stream
      (last-possible buffer operation)
      (let [stream-mark [i j]
            input (classify (get-in lines [i j]) big?)
            state' (nfa/advance state [stream-mark input])]
        (assert (not (nfa/reject? state')))
        (if (nfa/accept? state')
          [i j]
          (recur (next stream) state'))))))

(s/defmethod resolve/resolve-motion :word :- (s/maybe l/Location)
  [{:keys [lines point] :as buffer} {:keys [operator] n :count :as operation}]
  (let [point' (n-times point (or n 1) (partial next-word buffer operation))]
    (cond
      (= point point')
      nil

      (and (not= operator :move-point)
           (not= (first point) (first point')))
      [(first point) (count (get lines (first point)))]

      :else
      point')))
