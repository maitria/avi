(ns avi.t-nfa
  (:require [avi.nfa :refer :all]
            [midje.sweet :refer :all]))

(defn- state-after-inputs
  [nfa inputs]
  (reduce
    (fn [s input]
      (if (= :reject s)
        :reject
        (advance nfa s input :reject)))
    (start nfa)
    inputs))

(defn- characterize-state
  [nfa state]
  (cond
    (= :reject state)   :reject
    (accept? nfa state) :accept
    :else               :pending))

(tabular
  (facts "about NFAs"
    (let [nfa ?nfa
          state (state-after-inputs nfa ?inputs)
          result (characterize-state nfa state)]
      result => ?result))

  ?nfa                       ?inputs  ?result
  (match 1)                  []       :pending
  (match 1)                  [1]      :accept
  (match 1)                  [2]      :reject
  (match 1)                  [1 2]    :reject

  (any)                      []       :pending
  (any)                      [1]      :accept
  (any)                      [2]      :accept
  (any)                      [1 2]    :reject

  (opt (match 1))            []       :accept
  (opt (match 1))            [1]      :accept
  (opt (match 1))            [2]      :reject
  (opt (match 1))            [1 1]    :reject
  (opt (match 1))            [1 2]    :reject

  (alt (match 1))            []       :pending
  (alt (match 1))            [1]      :accept
  (alt (match 1))            [2]      :reject
  (alt (match 1))            [1 1]    :reject
  (alt (match 1))            [1 2]    :reject
  (alt (match 1) (match 2))  []       :pending
  (alt (match 1) (match 2))  [1]      :accept
  (alt (match 1) (match 2))  [2]      :accept
  (alt (match 1) (match 2))  [3]      :reject
  (alt (match 1) (match 2))  [1 1]    :reject
  (alt (match 1) (match 2))  [1 3]    :reject
  (alt (match 1) (match 2))  [3 1]    :reject
  (alt (match 1) (match 2))  [3 3]    :reject

  (kleene (match 1))         []       :accept
  (kleene (match 1))         [1]      :accept
  (kleene (match 1))         [1 1]    :accept
  (kleene (match 1))         [1 1 1]  :accept
  (kleene (match 1))         [2]      :reject
  (kleene (match 1))         [1 2]    :reject
  (kleene (match 1))         [1 1 2]  :reject

  )
