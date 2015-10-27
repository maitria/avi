(ns avi.render
  (:import [java.util Arrays])
  (:require [clojure.set :refer [map-invert]]
            [avi.editor :as e]
            [avi.buffer :as b]
            [avi.color :as color]))

(defn- render-message-line
  [editor]
  (cond
    (and (:prompt editor) (:command-line editor))
    [(color/make :white :black) (str (:prompt editor) (:command-line editor))]

    (:message editor)
    (let [[foreground background text] (:message editor)]
      [(color/make foreground background) text])

    :else
    [(color/make :white :black) ""]))

(defn- render-line
  [editor i]
  (let [[height] (:size (:viewport editor))
        buffer (e/current-buffer editor)
        top (:viewport-top buffer)
        message-line (dec height)
        status-line (dec message-line)
        last-edit-line (dec status-line)
        buffer-line (+ i top)
        buffer-line-count (b/line-count buffer)]
    (cond
      (= message-line i)
      (render-message-line editor)

      (= status-line i)
      [(color/make :black :white) (or (:name buffer) "[No Name]")]

      (< buffer-line buffer-line-count)
      (let [white-on-black (color/make :white :black)
            red-on-black (color/make :red :black)
            line (b/line buffer buffer-line)
            attrs (byte-array (count line) white-on-black)]
        (doseq [j (range (count line))]
          (when (#{\( \)} (get line j))
            (aset-byte attrs j red-on-black)))
        [attrs line])

      :else
      [(color/make :blue :black) "~"])))

(defmulti ^:private point-position :mode)

(defmethod point-position :default
  [editor]
  (let [buffer (e/current-buffer editor)
        [buffer-point-i buffer-point-j] (:point buffer)
        viewport-top (:viewport-top buffer)]
    [(- buffer-point-i viewport-top) buffer-point-j]))

(defmethod point-position :command-line
  [editor]
  (let [[height] (:size (:viewport editor))]
    [(dec height) (inc (count (:command-line editor)))]))

(let [byte-array-class (Class/forName "[B")]
  (defn byte-array?
    [obj]
    (= byte-array-class (class obj))))

(defn render
  [editor]
  (let [[height width] (:size (:viewport editor))
        default-attrs (color/make :white :black)
        rendered-chars (char-array (* height width) \space)
        rendered-attrs (byte-array (* height width) default-attrs)]
    (doseq [i (range height)]
      (let [[attrs text] (render-line editor i)]
        (.getChars text 0 (min width (count text)) rendered-chars (* i width))
        (if (byte-array? attrs)
          (System/arraycopy attrs 0 rendered-attrs (* i width) (min width (count attrs)))
          (Arrays/fill rendered-attrs (* i width) (* (inc i) width) attrs))))
    {:width width
     :chars rendered-chars
     :attrs rendered-attrs
     :point (point-position editor)}))

(defn rendered
  [editor]
  (assoc editor :rendition (render editor)))

(defn wrap
  [responder]
  (fn [editor event]
    (-> editor
      (responder event)
      rendered)))
