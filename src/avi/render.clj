(ns avi.render
  (:require [avi.editor :as e]
            [avi.buffer :as b]))

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

(defn- render-line
  [editor i]
  (let [[height] (:size editor)
        buffer (e/current-buffer editor)
        top (:viewport-top buffer)
        prompt-line (dec height)
        status-line (dec prompt-line)
        last-edit-line (dec status-line)
        buffer-line (+ i top)
        buffer-line-count (b/lines buffer)]
    (cond
      (= prompt-line i)
      [:white :black ""]

      (= status-line i)
      [:black :white (or (:name buffer) "[No Name]")]

      (< buffer-line buffer-line-count)
      [:white :black (b/line buffer buffer-line)]

      :else
      [:blue :black "~"])))

(defn- cursor-position
  [editor]
  (let [buffer (e/current-buffer editor)
        [buffer-cursor-i buffer-cursor-j] (:cursor buffer)
        viewport-top (:viewport-top buffer)]
    [(- buffer-cursor-i viewport-top) buffer-cursor-j]))

(defn render
  [editor]
  (let [[height width] (:size editor)
        rendered-chars (char-array (* height width) \space)
        rendered-attrs (byte-array (* height width) (make-attrs :white :black))]
    (doseq [i (range height)
            j (range width)]
      (let [index (+ j (* i width))
            [color background text] (render-line editor i)
            c (or (get text j) \space)]
        (aset rendered-chars index c)
        (aset rendered-attrs index (make-attrs color background))))
    {:width width
     :chars rendered-chars
     :attrs rendered-attrs
     :cursor (cursor-position editor)}))
