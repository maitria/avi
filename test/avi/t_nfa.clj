(ns avi.t-nfa
  (:require [avi.nfa :refer :all]
            [midje.sweet :refer :all]))

(defn- state-after-inputs
  [nfa inputs]
  (->> inputs
    (map vector (range))
    (reduce advance (start nfa))))

(defn f
  [v d]
  (+ (* 10 (or v 0)) (or d 0)))

(tabular
  (facts "about NFAs"
    (state-after-inputs ?nfa ?inputs) => (contains ?result))

  ?nfa                                         ?inputs  ?result
  (match 1)                                    []       {:status :pending}
  (match 1)                                    [1]      {:status :accept, :end 0}
  (match 1)                                    [2]      {:status :reject}
  (match 1)                                    [1 2]    {:status :reject}

  any                                          []       {:status :pending}
  any                                          [1]      {:status :accept, :end 0}
  any                                          [2]      {:status :accept, :end 0}
  any                                          [1 2]    {:status :reject}

  (maybe (match 1))                            []       {:status :accept}
  (maybe (match 1))                            [1]      {:status :accept, :end 0}
  (maybe (match 1))                            [2]      {:status :reject}
  (maybe (match 1))                            [1 1]    {:status :reject}
  (maybe (match 1))                            [1 2]    {:status :reject}

  (choice (match 1))                           []       {:status :pending}
  (choice (match 1))                           [1]      {:status :accept, :end 0}
  (choice (match 1))                           [2]      {:status :reject}
  (choice (match 1))                           [1 1]    {:status :reject}
  (choice (match 1))                           [1 2]    {:status :reject}
  (choice (match 1) (match 2))                 []       {:status :pending}
  (choice (match 1) (match 2))                 [1]      {:status :accept, :end 0}
  (choice (match 1) (match 2))                 [2]      {:status :accept, :end 0}
  (choice (match 1) (match 2))                 [3]      {:status :reject}
  (choice (match 1) (match 2))                 [1 1]    {:status :reject}
  (choice (match 1) (match 2))                 [1 3]    {:status :reject}
  (choice (match 1) (match 2))                 [3 1]    {:status :reject}
  (choice (match 1) (match 2))                 [3 3]    {:status :reject}

  (kleene (match 1))                           []       {:status :accept}
  (kleene (match 1))                           [1]      {:status :accept, :end 0}
  (kleene (match 1))                           [1 1]    {:status :accept}
  (kleene (match 1))                           [1 1 1]  {:status :accept}
  (kleene (match 1))                           [2]      {:status :reject}
  (kleene (match 1))                           [1 2]    {:status :reject}
  (kleene (match 1))                           [1 1 2]  {:status :reject}

  (chain (match 1) (match 2))                  []       {:status :pending}
  (chain (match 1) (match 2))                  [1]      {:status :pending}
  (chain (match 1) (match 2))                  [1 2]    {:status :accept, :end 1}
  (chain (match 1) (match 2))                  [1 2 3]  {:status :reject}
  (chain (match 1) (match 2))                  [3]      {:status :reject}
  (chain (match 1) (match 2))                  [1 3]    {:status :reject}

  (chain (maybe (match 1)) (match 2))          []       {:status :pending}
  (chain (maybe (match 1)) (match 2))          [1]      {:status :pending}
  (chain (maybe (match 1)) (match 2))          [1 2]    {:status :accept}
  (chain (maybe (match 1)) (match 2))          [2]      {:status :accept}
  (chain (kleene (match 1)) (match 2))         []       {:status :pending}
  (chain (kleene (match 1)) (match 2))         [1]      {:status :pending}
  (chain (kleene (match 1)) (match 2))         [2]      {:status :accept}
  (chain (kleene (match 1)) (match 2))         [1 2]    {:status :accept}
  (chain (kleene (match 1)) (match 2))         [1 1]    {:status :pending}
  (chain (kleene (match 1)) (match 2))         [1 1 2]  {:status :accept}
  (chain (match 1) (kleene (match 2)))         []       {:status :pending}
  (chain (match 1) (kleene (match 2)))         [1]      {:status :accept}
  (chain (match 1) (kleene (match 2)))         [1 2]    {:status :accept}
  (chain (match 1) (kleene (match 2)))         [1 2 2]  {:status :accept}
  (chain (match 1) (kleene (match 2)))         [1 2 1]  {:status :reject}

  (chain (kleene (on (match 1) f))
         (prune (match 2) #(= % 1)))           [1 2]    {:status :reject}
  (chain (kleene (on (match 1) f))
         (prune (match 2) #(= % 1)))           [1 1 2]  {:status :accept}
  (chain (kleene (on (match 1) f))
         (prune (match 2) #(= % 1)))           [2]      {:status :accept}

  (lookahead (match 1))                        []       {:status :pending}
  (lookahead (match 1))                        [1]      {:status :accept}
  (lookahead (match 1))                        [2]      {:status :reject}

  (chain (match 1) (lookahead (match 2)))      []       {:status :pending}
  (chain (match 1) (lookahead (match 2)))      [1]      {:status :pending}
  (chain (match 1) (lookahead (match 2)))      [1 2]    {:status :accept}
  (chain (match 1) (lookahead (match 2)))      [1 3]    {:status :reject}
  
  ;; NFAs reducing values
  (on (match 1) f)                             [1]      {:status :accept, :value 1}
  (on (match 2) f)                             [2]      {:status :accept, :value 2}

  (on any f)                                   [1]      {:status :accept, :value 1}
  (on any f)                                   [7]      {:status :accept, :value 7}

  (maybe (on (match 1) f))                     []       {:status :accept}
  (maybe (on (match 1) f))                     [1]      {:status :accept, :value 1}
  (on (maybe (match 1)) f)                     []       {:status :accept}
  (on (maybe (match 1)) f)                     [1]      {:status :accept, :value 1}

  (choice (on (match 1) f) (on (match 2) f))   [1]      {:status :accept, :value 1}
  (choice (on (match 1) f) (on (match 2) f))   [2]      {:status :accept, :value 2}
  (on (choice (match 1) (match 2)) f)          [1]      {:status :accept, :value 1}
  (on (choice (match 1) (match 2)) f)          [2]      {:status :accept, :value 2}

  (chain (on any f) any)                       [7 9]    {:status :accept, :value 7}

  (kleene (on (match 7) f))                    [7]      {:status :accept, :value 7}
  (kleene (on (match 7) f))                    [7 7]    {:status :accept, :value 77}
  (kleene (on (match 7) f))                    [7 7 7]  {:status :accept, :value 777}
  (kleene (on any f))                          [8 6 7]  {:status :accept, :value 867}

  (chain (on (match 1) f) (on (match 2) f))    [1 2]    {:status :accept, :value 12})
