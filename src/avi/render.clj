(ns avi.render
  (:import [java.util Arrays])
  (:require [clojure.set :refer [map-invert]]
            [avi.editor :as e]
            [avi.color :as color]
            [avi.layout :as layout]
            [avi.layout.panes :as p]))

(defn- point-position
  [{:keys [mode] :as editor}]
  (if (= mode :command-line)
    (let [[_ [height]] (::layout/shape editor)]
      [(dec height) (inc (count (:command-line editor)))])
    (p/point-position editor)))

(defn fill-rendition-line!
  [{:keys [width] rendered-chars :chars, rendered-attrs :attrs} n [[i j] [rows cols]] [attrs text]]
  (let [start (+ j (* (+ n i) width))
        text-size (count text)]
    (.getChars text 0 (min cols text-size) rendered-chars start)
    (Arrays/fill rendered-attrs start (+ start cols) attrs)))

(defmethod layout/render! ::p/pane
  [editor rendition {:keys [::p/lens] [[i j] [rows cols] :as shape] ::layout/shape}]
  (let [from-line i
        to-line (dec (+ i rows))
        document (get-in editor (e/current-document-path editor))]
    (doseq [i (range (inc (- to-line from-line)))]
      (let [{:keys [viewport-top] document-number :document} (get-in editor [:lenses lens])
            document-line (get-in editor [:documents document-number :lines (+ i viewport-top)])
            line-color (if document-line
                         (color/make :white :black)
                         (color/make :blue :black))
            line-text (or document-line "~")]
        (fill-rendition-line! rendition i shape [line-color line-text])))
    (let [file-name (or (:name document) "[No Name]")
          {:keys [viewport-top] [i j] :point  } (get-in editor [:lenses lens])
          num-lines (count (:lines document))
          pos-txt (if (= viewport-top 0)
                    (str "Top")
                    (if-not (< (+ viewport-top (dec rows)) num-lines)
                      (str "End")
                      (str (int (/ (* viewport-top 100) (- num-lines (dec rows)))) "%")))
          status-txt (str "  [" (inc i) "," (inc j) "]  " pos-txt)
          filelen (- cols (count status-txt))
          fmt-str (if (> filelen 0) (str "%-"filelen"."filelen"s" ) (str "%s"))
          msg-txt (str (format fmt-str file-name) status-txt)]
       (fill-rendition-line! rendition (dec rows) shape [(color/make :black :white) (str msg-txt)]))))

(defmethod layout/render! ::p/vertical-bar
  [editor rendition {[[i j] [rows cols] :as shape] ::layout/shape}]
  (let [color (color/make :black :white)]
    (doseq [n (range rows)]
      (fill-rendition-line! rendition n shape [color "|"]))))

(defn render-message-line!
  [editor rendition]
  (let [{:keys [::layout/shape]} editor
        [_ [height]] (::layout/shape editor)
        i (dec height)]
    (cond
      (and (:prompt editor) (:command-line editor))
      (fill-rendition-line! rendition i shape [(color/make :white :black) (str (:prompt editor) (:command-line editor))])

      (:message editor)
      (let [[foreground background text] (:message editor)]
        (fill-rendition-line! rendition i shape [(color/make foreground background) text])))))

(defn render
  [editor]
  (let [[_ [height width]] (::layout/shape editor)
        default-attrs (color/make :white :black)
        rendered-chars (char-array (* height width) \space)
        rendered-attrs (byte-array (* height width) default-attrs)
        rendition {:width width
                   :chars rendered-chars
                   :attrs rendered-attrs
                   :point (point-position editor)}]
    (run!
      #(layout/render! editor rendition %)
      (eduction layout/all-renderables [editor]))
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
