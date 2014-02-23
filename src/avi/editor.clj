(ns avi.editor)

(defn current-buffer
  [editor]
  (:buffer editor))

(defn update-current-buffer
  [editor buffer-fn]
  (assoc editor :buffer (buffer-fn (current-buffer editor))))
