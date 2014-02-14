(ns viv.core
  (:require [lanterna.screen :as lanterna]
            [viv.normal :as normal]
            [viv.buffer :as buffer])
  (:gen-class))

(defn start
  [[lines columns] filename]
  {:mode :normal
   :buffer (buffer/open filename)
   :lines lines
   :columns columns
   :count nil
   :beep? false})

(defn process
  [editor key]
  (normal/process editor key))

(defn render
  [editor]
  (let [buffer-lines (map #(vector :white :black %) (get-in editor [:buffer :lines]))
        tilde-lines (repeat [:blue :black "~"])
        status-line [:black :white (get-in editor [:buffer :name])]]
    {:lines (vec
              (concat
                (take (- (:lines editor) 2) (concat buffer-lines tilde-lines))
                [status-line])),
     :cursor (get-in editor [:buffer :cursor])}))

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
  (let [screen (lanterna/get-screen :text)]
    (lanterna/start screen)
    (loop [editor (let [[columns lines] (lanterna/get-size screen)]
                    (start [lines columns] filename))]
      (update-screen editor screen)
      (if-not (= (:mode editor) :finished)
        (recur (process editor (lanterna/get-key-blocking screen)))))
    (lanterna/stop screen)))
