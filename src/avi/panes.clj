(ns avi.panes
  (:require [clojure.spec :as s]))

(s/def ::nat (s/and int? (complement neg?)))
(s/def ::lens ::nat)
(s/def ::extent ::nat)
(s/def ::subtrees (s/coll-of ::tree :type vector?))
(s/def ::pane (s/keys :req [::lens]))
(s/def ::split (s/keys :req [::subtrees]))
(s/def ::tree (s/or :lens ::pane
                    :split ::split))
(s/def ::path (s/coll-of ::nat :type vector?))
(s/def ::shape (s/tuple (s/tuple ::nat ::nat)
                        (s/tuple ::nat ::nat)))

(s/fdef panes-to-render*
  :args (s/cat :shape ::shape
               :tree ::tree))
(defn- panes-to-render*
  [[[i j] [lines columns]] tree]
  (if (::lens tree)
    [{::lens (::lens tree)
      ::shape [[i j] [lines columns]]}]
    (let [{[{:keys [::extent] :as t} & ts] ::subtrees} tree
          this-pane-lines (or extent lines)]
      (concat
        (panes-to-render* [[i j] [this-pane-lines columns]] t)
        (if (seq ts)
          (panes-to-render* [[(+ i extent) j] [(- lines extent) columns]]
                            {::subtrees (vec ts)}))))))

(defn panes-to-render
  [{:keys [::tree] {[lines columns] :size} :viewport}]
  (panes-to-render* [[0 0] [(dec lines) columns]] tree))

(defn- shape
  [tree path [[i j] [rows cols] :as shape]]
  (if-not (empty? path)
    (let [[phead & prest] path
          {:keys [::extent] :as subnode} (first (::subtrees tree))]
      (if (zero? phead)
        (recur subnode prest [[i j] [(or extent rows) cols]])
        (recur
          (update tree ::subtrees (comp vec rest))
          (into [(dec phead)] prest)
          [[(+ i extent) j]
           [(- rows extent) cols]])))
    shape))

(defn- pane-area-shape
  "Shape of the rectangle where all panes are displayed.

  This accounts for the message line (ick)."
  [editor]
  (let [[rows cols] (get-in editor [:viewport :size])]
    [[0 0] [(dec rows) cols]]))

(defn current-pane-shape
  [{:keys [::tree ::path] :as editor}]
  (shape tree path (pane-area-shape editor)))

(defn current-pane-height
  [editor]
  (let [[_ [rows _]] (current-pane-shape editor)]
    rows))

(defn- pane-lens-id
  [panes path]
  (if (empty? path)
    (::lens panes)
    (recur
      (get-in panes [::subtrees (first path)])
      (rest path))))

(defn current-pane-lens-id
  [{:keys [::tree ::path]}]
  (pane-lens-id tree path))

(s/fdef split-pane
  :args (s/cat :panes ::tree
               :path ::path
               :new-lens ::lens
               :total-height ::nat)
  :ret ::split)
(defn split-pane
  [panes path new-lens total-height]
  (let [panes (if (::lens panes)
                [panes]
                (::subtrees panes))
        each-pane-height (int (/ total-height (inc (count panes))))
        panes (-> (mapv #(assoc % ::extent each-pane-height) panes)
                (into [{::lens new-lens}]))]
    {::subtrees panes}))

(defn move-down-pane
  [editor]
  (assoc editor ::path [1]))
