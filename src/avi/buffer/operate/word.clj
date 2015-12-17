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
  [ch]
  (cond
    (or (nil? ch) (Character/isWhitespace ch)) :ws
    (word-char? ch)                            :word
    :else                                      :other))

(defn classify-big
  [ch]
  (case (classify ch)
    :ws    :ws
    :word  :other
    :other :other))

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
  [{:keys [lines]} {:keys [operator]}]
  (let [i (dec (count lines))
        j (cond-> (count (peek lines))
            (= operator :move-point)
            dec)]
    [i j]))

(defn at-zero-length-line?
  [[[i1 j1] [i2 _]]]
  (and (= 0 j1) (not= i1 i2)))

(defn nfa-process
  [nfa classifier input-stream]
  (reductions
    (fn [[state _] input]
      (let [state' (nfa/advance nfa state (classifier input))]
        [state' input]))
    (nfa/start nfa)
    input-stream))

(defn next-word
  [{:keys [lines] :as buffer} {[_ {:keys [big? direction]}] :motion, :as operation} [i j]]
  (let [classifier (if big?
                     classify-big
                     classify)]
    (loop [[[i j] :as stream] ((if (= direction :backward)
                                 l/backward
                                 l/forward) [i j] (lines/line-length lines))
           state (nfa/start first-of-next-word-nfa)]
      (if-not stream
        (last-location buffer operation)
        (let [state' (nfa/advance first-of-next-word-nfa state (classifier (get-in lines [i j])))]
          (assert (not (nfa/reject? state')))
          (if (or (nfa/accept? first-of-next-word-nfa state')
                  (at-zero-length-line? stream))
            [i j]
            (recur (next stream) state')))))))

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
