(ns viv.core
  (:require [lanterna.screen :as s]
            [clojure.string :as string])
  (:gen-class))

(defn screen-line
  [viv i]
  (get (:screen viv) i))

(defn start
  [[lines columns] args]
  (let [file-lines (string/split (slurp (first args)) #"\n")
        display-lines (take (- lines 2) (concat file-lines (repeat "~")))]
    {:screen (vec (concat
                    display-lines
                    [(first args)]))}))

(defn- update-screen
  [viv scr]
  (let [lines (:screen viv)]
    (doseq [i (range (count lines))]
      (s/put-string scr 0 i (get lines i))))
  (s/redraw scr))

(defn -main
  [& args]
  (let [scr (s/get-screen :unix)]
    (s/start scr)
    (let [[columns lines] (s/get-size scr)]
      (-> (start [lines columns] args)
          (update-screen scr)))
    (s/get-key-blocking scr)
    (s/stop scr)))
