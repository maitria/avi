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

(def nfas
  {:first (nfa/choice
            (nfa/chain ws+ (nfa/choice word other))
            (nfa/chain other+ (nfa/choice
                                (nfa/chain (nfa/kleene ws) word)
                                (nfa/chain ws+ other)))
            (nfa/chain word+ (nfa/choice
                               (nfa/chain (nfa/kleene ws) other)
                               (nfa/chain ws+ word))))
   :last (nfa/choice
           (nfa/chain nfa/any (nfa/maybe ws+) word+ (nfa/lookahead (nfa/choice ws other)))
           (nfa/chain nfa/any (nfa/maybe ws+) other+ (nfa/lookahead (nfa/choice ws word))))})

(def generators
  {:forward l/forward
   :backward l/backward})

(defn stop-at-empty-lines
  [nfa dir]
  (nfa/choice
    (nfa/chain (nfa/kleene nfa/any) nl (cond-> nl
                                         (= dir :backward)
                                         nfa/lookahead))
    nfa))

(defn last-possible
  [{:keys [lines]} {:keys [operator] [_ {:keys [direction]}] :motion}]
  (if (= direction :backward)
    [0 0]
    (let [i (dec (count lines))
          j (cond-> (count (peek lines))
              (= operator :move-point)
              dec)]
      [i j])))

(defn end-of-eager-match
  [nfa stream classify]
  (loop [[[i j] :as stream] stream 
         state (nfa/start nfa)]
    (when stream
      (let [input (classify [i j])
            state' (nfa/advance state [[i j] input])]
        (assert (not (nfa/reject? state')))
        (if (nfa/accept? state')
          (:end state')
          (recur (next stream) state'))))))

(defn next-word
  [{:keys [lines] :as buffer} {[_ {:keys [empty-lines? position-in-word big? direction]}] :motion, :as operation} [i j]]
  (let [nfa-type (case [position-in-word direction]
                   ([:start :forward] [:end :backward]) :first
                   ([:end :forward] [:start :backward]) :last)
        nfa (cond-> (nfas nfa-type)
              empty-lines?
              (stop-at-empty-lines direction))
        stream-generator (generators direction)
        stream (stream-generator [i j] (lines/line-length lines))
        classify #(classify (get-in lines %) big?)]
    (or
      (end-of-eager-match nfa stream classify)
      (last-possible buffer operation))))

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
