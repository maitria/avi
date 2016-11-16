(ns avi.render
 (:import [java.util Arrays])
 (:require [clojure.set :refer [map-invert]]
           [avi.editor :as e]
           [avi.edit-context.lines :as lines]
           [avi.color :as color]
           [avi.layout :as layout]
           [avi.layout.panes :as p]))

(defn- point-position
  [{:keys [:avi.editor/mode] :as editor}]
  (if (= mode :command-line)
    (let [[_ [height]] (::layout/shape editor)]
      [(dec height) (inc (count (:command-line editor)))])
    (p/point-position editor)))

(defn copy-blit!
  [{rendition-width :width,
    rendered-chars :chars,
    rendered-attrs :attrs
    :as rendition}
   {:keys [::width ::foreground ::background ::text]
    [i j] ::position}]
  (let [start (+ j (* i rendition-width))
        text-size (count text)
        attrs (color/make foreground background)]
    (.getChars text 0 (min width text-size) rendered-chars start)
    (Arrays/fill rendered-attrs start (+ start width) attrs)
    rendition))

(defn- status-text
  [editor lens [rows cols]]
  (let [{:keys [:avi.lenses/viewport-top]
         document-number :avi.lenses/document
         [pi pj] :avi.lenses/point} (get-in editor [:avi.lenses/lenses lens])
        document (get-in editor [:avi.documents/documents document-number])
        text (:avi.documents/text document)
        lines (lines/content text)
        file-name (or (:avi.documents/name document) "[No Name]")
        num-lines (count lines)
        pos-txt (if (= viewport-top 0)
                  (str "Top")
                  (if-not (< (+ viewport-top (dec rows)) num-lines)
                    (str "End")
                    (str (int (/ (* viewport-top 100) (- num-lines (dec rows)))) "%")))
        status-txt (str "  [" (inc pi) "," (inc pj) "]  " pos-txt)
        filelen (- cols (count status-txt))
        fmt-str (if (> filelen 0) (str "%-"filelen"."filelen"s" ) (str "%s"))]
    (str (format fmt-str file-name) status-txt)))

(defmethod layout/blits ::p/pane
  [rendition renderable editor rf]
  (let [{:keys [::p/lens] [[i j] [rows cols] :as shape] ::layout/shape} renderable
        from-line i
        to-line (dec (+ i rows))
        {:keys [:avi.lenses/viewport-top]
         document-number :avi.lenses/document} (get-in editor [:avi.lenses/lenses lens])
        document (get-in editor [:avi.documents/documents document-number])
        text (:avi.documents/text document)
        lines (lines/content text)]
    (-> (reduce
          (fn [rendition n]
            (let [document-line (get lines (+ n viewport-top))
                  line-text (or document-line "~")
                  foreground (if document-line
                               :white
                               :blue)]
              (rf rendition {::position [(+ n i) j]
                             ::width cols
                             ::text line-text
                             ::foreground foreground
                             ::background :black})))
          rendition
          (range (inc (- to-line from-line))))
      (rf {::position [to-line j]
           ::width cols
           ::text (status-text editor lens [rows cols])
           ::foreground :black
           ::background :white}))))

(defmethod layout/blits ::p/vertical-bar
  [rendition renderable editor rf]
  (let [{[[i j] [rows cols] :as shape] ::layout/shape} renderable]
    (reduce
      (fn [rendition n]
        (rf rendition {::position [(+ i n) j]
                       ::width cols
                       ::text "|"
                       ::foreground :black
                       ::background :white}))
      rendition
      (range rows))))

(defn render-message-line!
  [editor rendition]
  (let [[_ [rows cols]] (::layout/shape editor)
        i (dec rows)
        blit (merge (cond
                      (and (:prompt editor) (:command-line editor))
                      {::position [i 0]
                       ::width cols
                       ::text (str (:prompt editor) (:command-line editor))
                       ::foreground :white
                       ::background :black}

                      (:message editor)
                      (let [[foreground background text] (:message editor)]
                        {::position [i 0]
                         ::width cols
                         ::text text
                         ::foreground foreground
                         ::background background})))]
    (when blit
      (copy-blit! rendition blit))))

(defn blits
  [editor]
  (fn [rf]
    (fn blits*
      ([] (rf))
      ([rendition] (rf rendition))
      ([rendition renderable]
       (layout/blits rendition renderable editor rf)))))

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
    (transduce
      (comp layout/all-renderables
            (blits editor))
      (completing copy-blit!)
      rendition
      [editor])
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
