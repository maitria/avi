(ns avi.keymap-test
  (:require [midje.sweet :refer :all]
            [avi.keymap :as k]))

(facts "about splitting a key sequences"
  (k/split-key-sequence "g") => ["g"]
  (k/split-key-sequence "gg") => ["g" "g"]
  (k/split-key-sequence "<C-U>") => ["<C-U>"]
  (k/split-key-sequence "<C-G>j<C-D>") => ["<C-G>" "j" "<C-D>"])
