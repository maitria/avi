(ns avi.editor)

(defn current-buffer
  [editor]
  (:buffer editor))

(defn update-current-buffer
  [editor buffer-fn]
  (update-in editor [:buffer] buffer-fn))
