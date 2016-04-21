(ns avi.panes)

(defn panes-to-render
  [{:keys [panes]}]
  [{:lens panes
    :offset [0 0]
    :size [9 8]}])
