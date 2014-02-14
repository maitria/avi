(ns viv.core
  (:require [lanterna.screen :as lanterna]
            [viv.editor :as editor]
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

(defn- ensure-line-length
  [line length]
  (let [line-length (count line)]
    (cond
      (= line-length length)
      line

      (> line-length length)
      (.substring line 0 length)
      
      :else
      (apply str line (repeat (- length line-length) \space)))))

(defn render
  [editor]
  (let [columns (:columns editor)
        buffer-lines (->> (:lines (editor/current-buffer editor))
                          (map #(ensure-line-length % columns))
                          (map #(vector :white :black %)))
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
