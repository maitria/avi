(ns avi.world)

(defprotocol World
  "Avi's interface to the world."
  (setup [this])
  (cleanup [this])
  (read-key [this])
  (beep [this])
  (terminal-size [this])
  (update-terminal [this rendering])
  (read-file [this filename])
  (write-file [this filename contents]))

(def ^:dynamic *world*)
