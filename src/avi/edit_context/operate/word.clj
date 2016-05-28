(ns avi.edit-context.operate.word
  (:require [avi.edit-context
              [lines :as lines]
              [locations :as l]]
            [avi.edit-context.operate.resolve :as resolve]
            [avi.nfa :as nfa]
            [avi.pervasive :refer :all]))

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
           (nfa/chain nfa/any (nfa/maybe ws+) other+ (nfa/lookahead (nfa/choice ws word))))

   :end-of-word (nfa/chain (nfa/kleene (nfa/match :word)) (nfa/lookahead (nfa/chain ws)))})

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

(defn move-word
  [{:keys [lines] :as edit-context}
   {[_ {:keys [empty-lines? position-in-word big? direction]}] :motion, :as operation}
   [i j]]
  (let [nfa-type (case [position-in-word direction]
                   ([:start :forward] [:end :backward]) :first
                   ([:end :forward] [:start :backward]) :last
                   ([:anywhere :backward]) :end-of-word
                   ([:anywhere :forward]) :end-of-word)
        nfa (cond-> (nfas nfa-type)
              empty-lines?
              (stop-at-empty-lines direction))
        stream-generator (generators direction)
        stream (stream-generator [i j] (lines/line-length lines))
        classify #(classify (get-in lines %) big?)]
    (or
      (end-of-eager-match nfa stream classify)
      (last-possible edit-context operation))))

(defmethod resolve/resolve-motion :word
  [{:keys [lines point] :as edit-context}
   {:keys [operator]
    [_ {:keys [weird-delete-clip? type]}] :motion
    n :count
    :as operation}]
  (let [point' (n-times point (or n 1) (partial move-word edit-context operation))]
    (cond
      (= point point')
      nil

      ; `w` doesn't delete past end-of-line
      (and weird-delete-clip?
           (not= operator :move-point)
           (not= (first point) (first point')))
      [point
       [(first point) (count (get lines (first point)))]]

      :else
      [point point'])))

(def end-of-word-motion {:motion [:in-word {:position-in-word :anywhere
                                            :direction :forward}]})

(def start-of-word-motion {:motion [:in-word {:position-in-word :anywhere
                                              :direction :backward}]})

(defmethod resolve/resolve-motion :in-word
  [{:keys [lines point] :as edit-context} _]
  [(move-word edit-context start-of-word-motion point)
   (move-word edit-context end-of-word-motion point)])
