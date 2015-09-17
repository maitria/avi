(ns avi.t-eventmap
  (:require [midje.sweet :refer :all]
            [avi.eventmap :as em]))

(facts "about splitting a string of commands"
  (em/split-string-of-commands "g") => ["g"]
  (em/split-string-of-commands "gg") => ["g" "g"]
  (em/split-string-of-commands "<C-U>") => ["<C-U>"]
  (em/split-string-of-commands "<C-G>j<C-D>") => ["<C-G>" "j" "<C-D>"])
