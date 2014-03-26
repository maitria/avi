(ns avi.eventmap-test
  (:require [midje.sweet :refer :all]
            [avi.eventmap :as em]))

(facts "about splitting a key sequences"
  (em/split-event-spec "g") => ["g"]
  (em/split-event-spec "gg") => ["g" "g"]
  (em/split-event-spec "<C-U>") => ["<C-U>"]
  (em/split-event-spec "<C-G>j<C-D>") => ["<C-G>" "j" "<C-D>"])
