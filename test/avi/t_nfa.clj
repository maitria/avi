(ns avi.t-nfa
  (:require [avi.nfa :refer :all]
            [midje.sweet :refer :all]))

(defn accepts
  [nfa inputs]
  (accept? nfa (reduce
                 (fn [state input]
                   (if (= state :reject)
                     :reject
                     (advance nfa state input :reject)))
                 (start nfa)
                 inputs)))

(facts "about match nfa"
  (fact "it doesn't start in an accept state"
    (accepts (match "1") []) => falsey)
  (fact "it accepts the supplied value"
    (accepts (match "1") ["1"]) => truthy))
