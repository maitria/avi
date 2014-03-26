(ns avi.keymap-test
  (:require [midje.sweet :refer :all]
            [avi.keymap :as k]))

(facts "about splitting a key sequences"
  (k/split-event-spec "g") => ["g"]
  (k/split-event-spec "gg") => ["g" "g"]
  (k/split-event-spec "<C-U>") => ["<C-U>"]
  (k/split-event-spec "<C-G>j<C-D>") => ["<C-G>" "j" "<C-D>"])
