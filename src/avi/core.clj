(ns avi.core
  (:import [avi.terminal Screen])
  (:require [avi.buffer :as b]
            [avi.normal :as normal]
            [avi.render :as render])
  (:gen-class))

(defn start
  [[lines columns] filename]
  {:mode :normal
   :buffer (b/open filename)
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

(defn- update-screen
  [editor]
  (let [{chars :chars,
         attrs :attrs,
         width :width,
         [i j] :cursor} (render/render editor)]
    (Screen/refresh i j width chars attrs)))

(defn- screen-size
  []
  (let [size (Screen/size)]
    [(get size 0) (get size 1)]))

(defn -main
  [filename]
  (Screen/start)
  (loop [[height width] (screen-size)
         editor (start [height width] filename)]
    (if (:beep? editor)
      (Screen/beep))
    (let [editor (if (or (not= width (:columns editor))
                         (not= height (:lines editor)))
                   (process editor [:resize [height width]])
                   editor)]
      (update-screen editor)
      (if-not (= (:mode editor) :finished)
        (recur
          (screen-size)
          (process editor [:keystroke (Screen/getch)])))))
  (Screen/stop))
