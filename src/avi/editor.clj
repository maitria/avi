(ns avi.editor)

(defn beep
  [editor]
  (assoc editor :beep? true))

(defn current-buffer
  [editor]
  (:buffer editor))

(defn update-current-buffer
  [editor buffer-fn]
  (update-in editor [:buffer] buffer-fn))
