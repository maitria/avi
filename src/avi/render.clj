(ns avi.render
  (:import [java.util Arrays])
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

(defn- prompt-line-text
  [editor]
  (cond
    (= (:mode editor) :command-line)
    (str ":" (:command-line editor))

    (= (:mode editor) :insert)
    "--INSERT--"

    :else
    ""))

(defn- render-line
  [editor i]
  (let [[height] (:size editor)
        buffer (e/current-buffer editor)
        top (:viewport-top buffer)
        prompt-line (dec height)
        status-line (dec prompt-line)
        last-edit-line (dec status-line)
        buffer-line (+ i top)
        buffer-line-count (b/line-count buffer)]
    (cond
      (= prompt-line i)
      [:white :black (prompt-line-text editor)]

      (= status-line i)
      [:black :white (or (:name buffer) "[No Name]")]

      (< buffer-line buffer-line-count)
      [:white :black (b/line buffer buffer-line)]

      :else
      [:blue :black "~"])))

(defmulti ^:private cursor-position :mode)

(defmethod cursor-position :default
  [editor]
  (let [buffer (e/current-buffer editor)
        [buffer-cursor-i buffer-cursor-j] (:cursor buffer)
        viewport-top (:viewport-top buffer)]
    [(- buffer-cursor-i viewport-top) buffer-cursor-j]))

(defmethod cursor-position :command-line
  [editor]
  (let [[height] (:size editor)]
    [(dec height) (inc (count (:command-line editor)))]))

(defn render
  [editor]
  (let [[height width] (:size editor)
        default-attrs (make-attrs :white :black)
        rendered-chars (char-array (* height width) \space)
        rendered-attrs (byte-array (* height width) default-attrs)]
    (doseq [i (range height)]
      (let [[color background text] (render-line editor i)
            attrs (make-attrs color background)]
        (.getChars text 0 (count text) rendered-chars (* i width))
        (if (not= attrs default-attrs)
          (Arrays/fill rendered-attrs (* i width) (* (inc i) width) attrs))))
    {:width width
     :chars rendered-chars
     :attrs rendered-attrs
     :cursor (cursor-position editor)}))
