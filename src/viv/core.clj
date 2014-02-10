(ns viv.core
  (:require [lanterna.screen :as lanterna]
            [clojure.string :as string])
  (:gen-class))

(defn start
  [[lines columns] filename]
  {:mode :normal
   :buffer-name filename
   :buffer-lines (string/split (slurp filename) #"\n")
   :cursor [0 0]
   :lines lines
   :columns columns})

(defn process-key
  [editor key]
  (cond
    (= key :enter)
    (assoc editor :mode :finished)

    (= key \j)
    (let [[i j] (:cursor editor)]
      (assoc editor :cursor [(inc i) j]))
    
    :else
    editor))

(defn render
  [editor]
  (let [buffer-lines (map #(vector :white :black %) (:buffer-lines editor))
        tilde-lines (repeat [:blue :black "~"])
        status-line [:black :white (:buffer-name editor)]]
    {:lines (vec
              (concat
                (take (- (:lines editor) 2) (concat buffer-lines tilde-lines))
                [status-line])),
     :cursor (:cursor editor)}))

(defn- update-screen
  [editor screen]
  (let [rendition (render editor)
        screen-lines (:lines rendition)
        [cursor-i cursor-j] (:cursor rendition)]
    (doseq [i (range (count screen-lines))]
      (let [[color background text] (get screen-lines i)]
        (lanterna/put-string screen 0 i text {:bg background, :fg color})))
    (lanterna/move-cursor screen cursor-j cursor-i)
    (lanterna/redraw screen)))

(defn -main
  [filename]
  (let [screen (lanterna/get-screen :unix)]
    (lanterna/start screen)
    (loop [editor (let [[columns lines] (lanterna/get-size screen)]
                    (start [lines columns] filename))]
      (update-screen editor screen)
      (if-not (= (:mode editor) :finished)
        (recur (process-key editor (lanterna/get-key-blocking screen)))))
    (lanterna/stop screen)))
