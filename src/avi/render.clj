(ns avi.render
  (:import [java.util Arrays])
  (:require [clojure.set :refer [map-invert]]
            [avi.editor :as e]
            [avi.color :as color]
            [avi.panes :as p]))

(defn- point-position
  [{:keys [mode] :as editor}]
  (if (= mode :command-line)
    (let [[height] (:size (:viewport editor))]
      [(dec height) (inc (count (:command-line editor)))])
    (let [{:keys [viewport-top] [i j] :point} (get-in editor (e/current-lens-path editor))
          [[top _] _] (::p/shape (p/current-pane editor))]
      [(+ (- i viewport-top) top) j])))

(defn fill-rendition-line!
  [{:keys [width] rendered-chars :chars, rendered-attrs :attrs} i [attrs text]]
  (.getChars text 0 (min width (count text)) rendered-chars (* i width))
  (Arrays/fill rendered-attrs (* i width) (* (inc i) width) attrs))

(defn render-pane!
  [editor rendition [from-line to-line] lens-number]
  (let [document (get-in editor (e/current-document-path editor))]
    (doseq [i (range (inc (- to-line from-line)))]
      (let [{:keys [viewport-top] document-number :document} (get-in editor [:lenses lens-number])
            document-line (get-in editor [:documents document-number :lines (+ i viewport-top)])
            line-color (if document-line
                         (color/make :white :black)
                         (color/make :blue :black))
            line-text (or document-line "~")]
        (fill-rendition-line! rendition (+ i from-line) [line-color line-text])))
    (fill-rendition-line! rendition to-line [(color/make :black :white) (or (:name document) "[No Name]")])))

(defn render-message-line!
  [editor rendition]
  (let [[height] (:size (:viewport editor))
        i (dec height)]
    (cond
      (and (:prompt editor) (:command-line editor))
      (fill-rendition-line! rendition i [(color/make :white :black) (str (:prompt editor) (:command-line editor))])

      (:message editor)
      (let [[foreground background text] (:message editor)]
        (fill-rendition-line! rendition i [(color/make foreground background) text])))))

(defn render
  [editor]
  (let [[height width] (:size (:viewport editor))
        default-attrs (color/make :white :black)
        rendered-chars (char-array (* height width) \space)
        rendered-attrs (byte-array (* height width) default-attrs)
        rendition {:width width
                   :chars rendered-chars
                   :attrs rendered-attrs
                   :point (point-position editor)}]
    (transduce
      p/all-panes
      (fn
        ([])
        ([x] x)
        ([_ {:keys [::p/lens] [[i j] [lines columns]] ::p/shape}]
         (render-pane! editor rendition [i (dec (+ lines i))] lens)))
      (p/augmented-root-panes editor))

    (render-message-line! editor rendition)
    rendition))

(defn rendered
  [editor]
  (assoc editor :rendition (render editor)))

(defn wrap
  [responder]
  (fn [editor event]
    (-> editor
      (responder event)
      rendered)))
