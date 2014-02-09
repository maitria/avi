(ns viv.core
  (:require [lanterna.screen :as s])
  (:gen-class))

(defn -main
  []
  (let [scr (s/get-screen :unix)]
    (s/start scr)
    (s/get-key-blocking scr)
    (s/stop scr)))
