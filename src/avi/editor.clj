(ns avi.editor
  (:require [avi.compose :refer [->*]]
            [avi.buffer :as b]))

(defn initial-editor
  [[lines columns] & [filename]]
  {:mode :normal
   :buffer (b/open filename (- lines 2))
   :viewport {:size [lines columns]}
   :count nil
   :beep? false})

(defn beep
  [editor]
  (assoc editor :beep? true))

(defn current-buffer
  [editor]
  (:buffer editor))

(defn update-current-buffer
  [editor buffer-fn]
  (update-in editor [:buffer] buffer-fn))

(defn- valid-line?
  [editor i]
  (and (>= i 0)
       (< i (b/line-count (current-buffer editor)))))

(defn change-line
  [editor i-fn]
  (let [[i] (b/cursor (current-buffer editor))
        i (i-fn i)]
    (if-not (valid-line? editor i)
      (beep editor)
      (->* editor update-current-buffer
           (b/move-to-line i)))))

(defmulti respond
  (fn [editor [event-kind]]
    (if (= :resize event-kind)
      :resize
      (:mode editor))))
