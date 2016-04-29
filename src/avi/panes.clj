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

(defn- internal-pane-height
  [panes slot outer-pane-height]
  (if (= (inc slot) (count panes))
    (- outer-pane-height
       (->> panes
         rest
         (partition 1 2)
         flatten
         (reduce +)))
    (get panes (inc slot))))

(defn height
  [panes pane-path pane-height]
  (if (empty? pane-path)
    pane-height
    (let [slot (inc (* 2 (first pane-path)))]
      (recur
        (get panes slot)
        (rest pane-path)
        (internal-pane-height panes slot pane-height)))))

(defn current-pane-height
  [{:keys [panes pane-path] :as editor}]
  (height panes pane-path (dec (get-in editor [:viewport :size 0]))))
