(ns viv.core
  (:require [lanterna.screen :as lanterna]
            [clojure.string :as string])
  (:gen-class))

(defn start
  [[lines columns] filename]
  {:buffer-name filename
   :buffer-lines (string/split (slurp filename) #"\n")
   :lines lines
   :columns columns})

(defn render
  [editor]
  (let [buffer-lines (map #(vector :white :black %) (:buffer-lines editor))
        tilde-lines (repeat [:blue :black "~"])
        status-line [:black :white (:buffer-name editor)]]
    (vec
      (concat
        (take (- (:lines editor) 2) (concat buffer-lines tilde-lines))
        [status-line]))))

(defn- update-screen
  [editor screen]
  (let [screen-lines (render editor)]
    (doseq [i (range (count screen-lines))]
      (let [[color background text] (get screen-lines i)]
        (lanterna/put-string screen 0 i text {:bg background, :fg color}))))
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
