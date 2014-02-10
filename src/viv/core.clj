(ns viv.core
  (:require [lanterna.screen :as lanterna]
            [clojure.string :as string])
  (:gen-class))

(defn screen-line
  [editor i]
  (get (:screen editor) i))

(defn start
  [[lines columns] filename]
  (let [file-lines (->> (string/split (slurp filename) #"\n")
                        (map #(vector :white :black %)))
        tilde-lines (repeat [:blue :black "~"]) 
        display-lines (take (- lines 2) (concat file-lines tilde-lines))]
    {:screen (vec (concat
                    display-lines
                    [[:black :white filename]]))}))

(defn- update-screen
  [editor screen]
  (let [lines (:screen editor)]
    (doseq [i (range (count lines))]
      (let [[color background text] (get lines i)]
        (lanterna/put-string screen 0 i text {:bg background
                                              :fg color}))))
  (lanterna/redraw screen))

(defn -main
  [filename]
  (let [screen (lanterna/get-screen :unix)]
    (lanterna/start screen)
    (let [[columns lines] (lanterna/get-size screen)]
      (-> (start [lines columns] filename)
          (update-screen screen)))
    (lanterna/get-key-blocking screen)
    (lanterna/stop screen)))
