(ns avi.editor
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]))

(defn initial-editor
  [[lines columns] [filename]]
  {:old-mode :normal
   :buffer (b/open filename (- lines 2))
   :viewport {:size [lines columns]}
   :count nil
   :beep? false})

(def finished? :finished?)

(defn beep
  [editor]
  (assoc editor :beep? true))

(defn current-buffer
  ([editor]
   (:buffer editor))
  ([editor buffer-fn]
   (update-in editor [:buffer] buffer-fn)))

(defn- valid-line?
  [editor i]
  (and (>= i 0)
       (< i (b/line-count (current-buffer editor)))))

(defn change-line
  [editor i-fn]
  (+> editor
      (let [[i] (:cursor (current-buffer editor))
        i (i-fn i)]
        (if-not (valid-line? editor i)
          (beep)
          (in current-buffer
              (b/move-to-line i))))))

(defn enter-normal-mode
  [editor]
  (+> editor
    (assoc :old-mode :normal, :message nil)
    (dissoc :mode)))

(defn wrap-handle-resize
  [responder]
  (fn [editor [event-type size :as event]]
    (if (= event-type :resize)
      (+> editor
          (assoc-in [:viewport :size] size)
          (in current-buffer
              (b/resize (- (first size) 2))))
      (responder editor event))))

(defn wrap-handle-exceptions
  [responder]
  (fn [editor event]
    (try
      (responder editor event)
      (catch Throwable e
        (merge editor (ex-data e))))))
