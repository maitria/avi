(ns avi.t-nfa
  (:require [avi.nfa :as nfa]
            [midje.sweet :refer :all]))

(facts "about match nfa"
  (fact "it doesn't start in an accept state"
    (nfa/accept? (nfa/start (nfa/match "1"))) => falsey))
