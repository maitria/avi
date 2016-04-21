(ns avi.panes)

(defn- panes-to-render*
  [[i j] [lines columns] tree]
  (if (number? tree)
    [{:lens tree
      :offset [i j]
      :size [lines columns]}]
    (let [[_ lens-a offset-adjust lens-b] tree]
      (concat
        (panes-to-render* [i j] [offset-adjust columns] lens-a)
        (panes-to-render* [(+ i offset-adjust) j]
                          [(- lines offset-adjust) columns]
                          lens-b)))))

(defn panes-to-render
  [{:keys [panes] {[lines columns] :size} :viewport :as editor}]
  (panes-to-render* [0 0] [(dec lines) columns] panes))
