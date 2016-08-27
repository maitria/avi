(ns avi.layout.panes
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

(defn- root-pane-shape
  "Shape of the rectangle where all panes are displayed.

  This accounts for the message line (ick)."
  [editor]
  (let [[rows cols] (get-in editor [:viewport :size])]
    [[0 0] [(dec rows) cols]]))

(defn- tag-pane
  [pane]
  (assoc pane :avi.layout/renderable-type :avi.layout.panes/pane))

(defn all-panes
  "A transducer which visits all leaf nodes (panes) in a pane tree.

  The input pane trees must be augmented with :avi.layout/shape and ::path,
  but only at the tree's root.  (See augmented-root-panes.) This information
  is used to augment each sub-pane before rf is applied to it."
  [rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (if (::lens input)
       (rf result (tag-pane input))
       (let [{:keys [::subtrees :avi.layout/shape ::path]} input]
         (reduce
           (fn [[result [[i j] [rows cols]] n] {:keys [::extent] :as input}]
             (let [height (or extent rows)
                   input (assoc input
                                :avi.layout/shape [[i j] [height cols]]
                                ::path (conj path n))
                   result (rf result (tag-pane input))]
               [result [[(+ i height) j] [(- rows height) cols]] (inc n)]))
           [result shape 0]
           subtrees))))))

(defn augmented-root-panes
  [{:keys [::tree] :as editor}]
  [(assoc tree
          :avi.layout/shape (root-pane-shape editor)
          ::path [])])

(defn current-pane
  [{:keys [::path] :as editor}]
  (first (sequence
           (comp all-panes (filter #(= (::path %) path)))
           (augmented-root-panes editor))))

(defn point-position
  "Position of the point in the currently focused pane.
  
  (Note: The cursor could be elsewhere if something else is focused, like the
  command-line.)"
  [editor]
  (let [{:keys [::lens] [[top _] _] :avi.layout/shape :as pane}
          (current-pane editor)
        {:keys [viewport-top] [i j] :point} (get-in editor [:lenses lens])]
    [(+ (- i viewport-top) top) j]))

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
