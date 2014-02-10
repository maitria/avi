(ns viv.core
  (:require [lanterna.screen :as screen]
            [clojure.string :as string])
  (:gen-class))

(defn screen-line
  [editor i]
  (get (:screen editor) i))

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
  [editor scr]
  (let [lines (:screen editor)]
    (doseq [i (range (count lines))]
      (let [[color background text] (get lines i)]
        (screen/put-string scr 0 i text {:bg background
                                    :fg color}))))
  (screen/redraw scr))

(defn -main
  [& args]
  (let [scr (screen/get-screen :unix)]
    (screen/start scr)
    (let [[columns lines] (screen/get-size scr)]
      (-> (start [lines columns] args)
          (update-screen scr)))
    (screen/get-key-blocking scr)
    (screen/stop scr)))
