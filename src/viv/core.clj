(ns viv.core
  (:require [lanterna.screen :as s]
            [clojure.string :as string])
  (:gen-class))

(defn screen-line
  [viv i]
  (get (:screen viv) i))

(defn start
  [[lines columns] args]
  (let [file-lines (->> (string/split (slurp (first args)) #"\n")
                        (map #(vector :white :black %)))
        tilde-lines (repeat [:blue :black "~"]) 
        display-lines (take (- lines 2) (concat file-lines tilde-lines))]
    {:screen (vec (concat
                    display-lines
                    [[:black :white (first args)]]))}))

(defn- update-screen
  [viv scr]
  (let [lines (:screen viv)]
    (doseq [i (range (count lines))]
      (let [[color background text] (get lines i)]
        (s/put-string scr 0 i text {:bg background
                                    :fg color}))))
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
