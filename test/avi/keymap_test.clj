(ns avi.keymap-test
  (:require [midje.sweet :refer :all]
            [avi.keymap :as k]))

(facts "about splitting a key sequences"
  (k/split-key-sequence "g") => ["g"])
