(ns viv.core
  (:require [lanterna.screen :as s])
  (:gen-class))

(defn screen-line
  [viv i]
  (get (:screen viv) i))

(defn start
  [[lines columns] args]
  {:screen [(slurp (first args))]})

(defn- update-screen
  [viv scr]
  (let [lines (:screen viv)]
    (doseq [i (range (count lines))]
      (s/put-string scr i 0 (get lines i))))
  (s/redraw scr))

(defn -main
  [& args]
  (let [scr (s/get-screen :unix)]
    (s/start scr)
    (-> (start (s/get-size scr) args)
        (update-screen scr))
    (s/get-key-blocking scr)
    (s/stop scr)))
