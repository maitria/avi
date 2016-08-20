(ns avi.panes
  (:require [clojure.spec :as s]))

(s/def ::nat (s/and int? (complement neg?)))
(s/def ::direction #{:h :v})
(s/def ::lens ::nat)
(s/def ::extent ::nat)

(s/def ::split (s/cat :direction ::direction
                      :middle (s/* (s/cat :tree ::tree
                                          :extent ::extent))
                      :lens (s/keys :req [::lens])))
(s/def ::tree (s/or :lens (s/keys :req [::lens])
                    :split ::split))

(s/def ::path (s/coll-of ::nat :type vector?))

(s/fdef panes-to-render*
  :args (s/cat :offset (s/tuple ::nat ::nat)
               :size (s/tuple ::nat ::nat)
               :tree ::tree))
(defn- panes-to-render*
  [[i j] [lines columns] tree]
  (if (::lens tree)
    [{:lens (::lens tree)
      :offset [i j]
      :size [lines columns]}]
    (let [[direction lens-a offset-adjust & rest-of-panes] tree
          this-pane-lines (if offset-adjust
                            offset-adjust
                            lines)]
      (concat
        (panes-to-render* [i j] [this-pane-lines columns] lens-a)
        (if (seq rest-of-panes)
          (panes-to-render* [(+ i offset-adjust) j]
                            [(- lines offset-adjust) columns]
                            (into [direction] rest-of-panes)))))))

(defn panes-to-render
  [{:keys [::tree] {[lines columns] :size} :viewport}]
  (panes-to-render* [0 0] [(dec lines) columns] tree))

(s/fdef internal-pane-height
  :args (s/and (s/cat :panes ::tree
                      :slot ::nat
                      :outer-pane-height ::nat)
               (fn slot-valid? [{:keys [panes slot]}]
                 (< slot (count (s/unform ::tree panes)))))
  :ret ::nat)
(defn- internal-pane-height
  [panes slot outer-pane-height]
  (if (= (inc slot) (count panes))
    (- outer-pane-height
       (->> panes
         (drop 2)
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
  [{:keys [::tree pane-path] :as editor}]
  (height tree pane-path (dec (get-in editor [:viewport :size 0]))))

(defn internal-pane-top
  [panes pane-number outer-pane-top]
  (+ outer-pane-top
     (->> panes
       (partition 1 2)
       rest
       flatten
       (take pane-number)
       (reduce +))))

(s/fdef top
  :args (s/cat :panes ::tree
               :pane-path ::path
               :pane-height ::nat
               :pane-top ::nat)
  :ret ::nat)
(defn top
  [panes pane-path pane-height pane-top]
  (if (empty? pane-path)
    pane-top
    (let [slot (inc (* 2 (first pane-path)))]
      (recur
        (get panes slot)
        (rest pane-path)
        (internal-pane-height panes slot pane-height)
        (internal-pane-top panes (first pane-path) pane-top)))))

(defn current-pane-top
  [{:keys [::tree pane-path] :as editor}]
  (top tree pane-path (dec (get-in editor [:viewport :size 0])) 0))

(defn- pane-lens-id
  [panes pane-path]
  (if (empty? pane-path)
    (::lens panes)
    (recur
      (get panes (inc (* 2 (first pane-path))))
      (rest pane-path))))

(defn current-pane-lens-id
  [{:keys [::tree pane-path]}]
  (pane-lens-id tree pane-path))

(defn split-pane
  [panes pane-path new-lens total-height]
  (let [panes (if (::lens panes)
                [:h panes]
                panes)
        pane-count (inc (/ (count panes) 2))
        each-pane-height (int (/ total-height pane-count))
        panes-with-split (into panes [each-pane-height {::lens new-lens}])
        size-slots (take-while #(get panes-with-split %) (iterate (partial + 2) 2))
        panes-with-normalized-sizes (reduce
                                      #(assoc %1 %2 each-pane-height)
                                      panes-with-split
                                      size-slots)]
    panes-with-normalized-sizes))

(defn move-down-pane
  [editor]
  (assoc editor :pane-path [1]))
