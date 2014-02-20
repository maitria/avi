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

(defn- color-number
  [color]
  (case color
    :black 0
    :red 1
    :green 2
    :yellow 3
    :blue 4
    :magenta 5
    :cyan 6
    :white 7))

(defn make-attrs
  [color background]
  (byte (bit-or (bit-shift-left (color-number color) 3)
                (color-number background))))

(defn render
  [editor]
  (let [lines (:lines editor)
        width (:columns editor)
        rendered-chars (char-array (* lines width) \space)
        rendered-attrs (byte-array (* lines width) (make-attrs :white :black))
        buffer-lines (->> (:lines (editor/current-buffer editor))
                          (map #(vector :white :black %)))
        tilde-lines (repeat [:blue :black "~"])
        status-line [:black :white (get-in editor [:buffer :name])]
        prompt-line [:white :black ""]
        lines (vec
                (concat
                  (take (- (:lines editor) 2) (concat buffer-lines tilde-lines))
                  [status-line]
                  [prompt-line]))]
    (doseq [i (range (count lines))
            j (range width)]
      (let [index (+ j (* i width))
            [color background text] (get lines i)
            c (or (get text j) \space)]
        (aset rendered-chars index c)
        (aset rendered-attrs index (make-attrs color background))))
    {:width width
     :chars rendered-chars
     :attrs rendered-attrs
     :lines lines
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
