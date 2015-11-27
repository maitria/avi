(ns avi.t-nfa
  (:require [avi.nfa :refer :all]
            [midje.sweet :refer :all]))

(facts "about match nfa"
  (fact "it doesn't start in an accept state"
    (let [nfa (match "1")]
      (accept? nfa (start nfa))) => falsey)
  (fact "it accepts the supplied value"
    (let [nfa (match "1")
          adv #(advance nfa %1 %2 :reject)]
      (accept? nfa (-> (start nfa) (adv "1"))) => truthy)))
