(ns avi.panes)

(defn panes-to-render
  [{:keys [panes] {[lines columns] :size} :viewport :as editor}]
  [{:lens panes
    :offset [0 0]
    :size [(dec lines) columns]}])
