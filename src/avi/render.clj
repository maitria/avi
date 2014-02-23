(ns avi.render
  (:require [avi.editor :as editor]))

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
        prompt-line (dec height)
        status-line (dec prompt-line)
        last-edit-line (dec status-line)
        last-file-line (min last-edit-line (dec (count (:lines (editor/current-buffer editor)))))]
    (cond
      (= prompt-line i)
      [:white :black ""]

      (= status-line i)
      [:black :white (:name (editor/current-buffer editor))]

      (<= i last-file-line)
      [:white :black (get (:lines (editor/current-buffer editor)) i)]

      :else
      [:blue :black "~"])))

(defn render
  [editor]
  (let [lines (:lines editor)
        width (:columns editor)
        rendered-chars (char-array (* lines width) \space)
        rendered-attrs (byte-array (* lines width) (make-attrs :white :black))]
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
     :cursor (get-in editor [:buffer :cursor])}))
