(ns avi.beep)

(defn beep
  [editor-like]
  (assoc editor-like :beep? true))

(defn error
  [editor-like message]
  (assoc editor-like :message [:white :red message]))
