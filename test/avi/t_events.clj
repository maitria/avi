(ns avi.t-events
  (:require [midje.sweet :refer :all]
            [avi.events :as ev]))

(facts "about splitting a string of commands"
  (ev/split-string-of-commands "g") => ["g"]
  (ev/split-string-of-commands "gg") => ["g" "g"]
  (ev/split-string-of-commands "<C-U>") => ["<C-U>"]
  (ev/split-string-of-commands "<C-G>j<C-D>") => ["<C-G>" "j" "<C-D>"])
