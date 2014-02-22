(ns viv.core
  (:import [viv.terminal Screen])
  (:require [lanterna.screen :as lanterna]
            [viv.buffer :as buffer]
            [viv.editor :as editor]
            [viv.normal :as normal]
            [viv.render :as render])
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
  [editor [event-kind event-data]]
  (cond
    (= :keystroke event-kind)
    (normal/process editor event-data)

    (= :resize event-kind)
    (let [[lines columns] event-data]
      (-> editor
          (assoc :lines lines)
          (assoc :columns columns)))

    :else
    editor))

(defn- update-screen
  [editor screen]
  (let [rendition (render/render editor)
        screen-lines (:lines rendition)
        [cursor-i cursor-j] (:cursor rendition)]
    (doseq [i (range (count screen-lines))]
      (let [[color background text] (get screen-lines i)]
        (lanterna/put-string screen 0 i text {:bg background, :fg color})))
    (lanterna/move-cursor screen cursor-j cursor-i)
    (lanterna/redraw screen)))

(defn -main
  [filename]
  (let [s (Screen.)
        screen (lanterna/get-screen :text)]
    (lanterna/start screen)
    (loop [[columns lines] (lanterna/get-size screen)
           editor (start [lines columns] filename)]
      (let [editor (if (or (not= columns (:columns editor))
                           (not= lines (:lines editor)))
                     (process editor [:resize [lines columns]])
                     editor)]
        (update-screen editor screen)
        (if-not (= (:mode editor) :finished)
          (recur
            (lanterna/get-size screen)
            (process editor [:keystroke (lanterna/get-key-blocking screen)])))))
    (lanterna/stop screen)))
