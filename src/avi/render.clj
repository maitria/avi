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
  (let [height (:lines editor)
        buffer (e/current-buffer editor)
        [top] (:viewport-offset buffer)
        prompt-line (dec height)
        status-line (dec prompt-line)
        last-edit-line (dec status-line)
        buffer-line (+ i top)
        buffer-line-count (b/lines buffer)]
    (cond
      (= prompt-line i)
      [:white :black ""]

      (= status-line i)
      [:black :white (:name buffer)]

      (< buffer-line buffer-line-count)
      [:white :black (b/line buffer buffer-line)]

      :else
      [:blue :black "~"])))

(defn render
  [editor]
  (let [lines (:lines editor)
        width (:columns editor)
        rendered-chars (char-array (* lines width) \space)
        rendered-attrs (byte-array (* lines width) (make-attrs :white :black))
        buffer (e/current-buffer editor)
        [buffer-cursor-i buffer-cursor-j] (:cursor buffer)
        [viewport-offset-i viewport-offset-j] (:viewport-offset buffer)
        cursor [(- buffer-cursor-i viewport-offset-i)
                (- buffer-cursor-j viewport-offset-j)]]
    (doseq [i (range lines)
            j (range width)]
      (let [index (+ j (* i width))
            [color background text] (render-line editor i)
            c (or (get text j) \space)]
        (aset rendered-chars index c)
        (aset rendered-attrs index (make-attrs color background))))
    {:width width
     :chars rendered-chars
     :attrs rendered-attrs
     :cursor cursor}))
