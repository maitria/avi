(ns avi.layout.panes
  (:require [avi.beep :as b]
            [avi.xforms :as xf]
            [clojure.spec :as s]
            [packthread.core :refer :all]))

(s/def ::lens nat-int?)
(s/def ::extent nat-int?)
(s/def ::subtrees (s/coll-of ::tree :type vector?))
(s/def ::pane (s/keys :req [::lens]))
(s/def ::split (s/keys :req [::subtrees]))
(s/def ::tree (s/or :lens ::pane
                    :split ::split))
(s/def ::path (s/coll-of nat-int? :type vector?))

(s/def ::editor (s/keys :req [::tree ::path :avi.layout/shape]))

(defn- root-pane-shape
  "Shape of the rectangle where all panes are displayed.

  This accounts for the message line (ick)."
  [editor]
  (let [[[i j] [rows cols]] (:avi.layout/shape editor)]
    [[0 0] [(dec rows) cols]]))

(defn- annotate-shape
  [parent]
  (xf/annotate (fn [[[i j] [rows cols]] input]
                 (let [height (or (::extent input) rows)]
                   [[[(+ i height) j] [(- rows height) cols]]
                    (assoc input :avi.layout/shape [[i j] [height cols]])]))
               (:avi.layout/shape parent)))

(defn- annotate-path
  [parent]
  (xf/annotate (fn [state input]
                 [(inc state) (assoc input ::path (conj (::path parent) state))])
               0))

(def annotate-renderable-type
  (map #(cond-> %
          (::lens %)
          (assoc :avi.layout/renderable-type :avi.layout.panes/pane))))

(defn- annotate
  [parent]
  (comp
    (annotate-shape parent)
    (annotate-path parent)
    annotate-renderable-type))

(def ^:private deannotate
  (map (fn [node]
         (dissoc node
                 :avi.layout/renderable-type
                 :avi.layout/shape
                 ::path))))

(defn- xfmap
  ([xform tree]
   (cond-> tree
     (::subtrees tree)
     (update ::subtrees #(into [] (comp (annotate tree) xform deannotate) %))))
  ([result rf tree]
   (transduce (annotate tree) rf result (::subtrees tree))))

(defn all-nodes
  "A transducer which visits all nodes in a pane tree.

  The input pane trees must be augmented with :avi.layout/shape and ::path,
  but only at the tree's root.  (See augmented-root-panes.) This information
  is used to augment each child node before rf is applied to it."
  [rf]
  (fn all-nodes-rf
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (-> result
       (xfmap all-nodes-rf input)
       (rf input)))))

(def all-panes (comp all-nodes (filter ::lens)))

(defn- augmented-root-pane
  [{:keys [::tree] :as editor}]
  (assoc tree
         :avi.layout/shape (root-pane-shape editor)
         ::path []))

(defn augmented-root-panes
  [editor]
  [(augmented-root-pane editor)])

(defn current-pane
  [{:keys [::path] :as editor}]
  (first (sequence
           (comp all-nodes (filter #(= (::path %) path)))
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

(defn pane-tree-cata
  [editor xform]
  (let [tree (augmented-root-pane editor)
        tree (first (xf/cata
                      xfmap
                      (comp
                        (map #(cond-> %
                                (= (::path editor) (::path %))
                                (assoc ::focused true)))
                        xform)
                      tree))
        path (::path (first (sequence
                              (comp all-nodes (filter ::focused))
                              [(assoc tree ;; hack! see augmented-root-pane
                                      :avi.layout/shape (root-pane-shape editor)
                                      ::path [])])))
        _ (assert path)
        tree (first (xf/cata xfmap (map #(dissoc % ::focused)) tree))]
    (assoc editor
           ::path path
           ::tree tree)))

(defn- remove-last-child-extent
  [tree]
  (+> tree
    (if (::subtrees tree)
      (let [n (count (::subtrees tree))]
        (update-in [::subtrees (dec n)] dissoc ::extent)))))

(defn- assign-equal-extents
  [tree]
  (+> tree
    (if-let [subtrees (::subtrees tree)]
      (let [{[[i j] [rows cols]] :avi.layout/shape} tree
            pane-height (int (/ (dec rows) (count subtrees)))]
        (assoc ::subtrees
               (into []
                     (map #(assoc % ::extent pane-height))
                     subtrees))))))

(defn resize-panes
  "Makes all panes equal size."
  [editor]
  (pane-tree-cata editor (comp (map assign-equal-extents)
                               (map remove-last-child-extent))))

(defn- flatten-like-splits
  [tree]
  (+> tree
   (if-let [subtrees (::subtrees tree)]
     (update ::subtrees
             #(vec
                (mapcat (fn [child]
                          (if (::lens child)
                            [child]
                            (::subtrees child)))
                        %))))))

(defn- remove-one-child-splits
  [tree]
  (+> tree
    (if (= 1 (count (::subtrees tree)))
      (get-in [::subtrees 0]))))

(defn- simplify-panes
  [editor]
  (pane-tree-cata editor (comp (map flatten-like-splits)
                               (map remove-one-child-splits))))

(defn- split-pane'
  [editor new-lens]
  (+> editor
    (pane-tree-cata
      (map (fn [node]
             (if (::focused node)
               (let [[_ [rows _]] (:avi.layout/shape node)
                     new-height (max 2 (int (/ rows 2)))]
                 {::path (::path node)
                  :avi.layout/shape (:avi.layout/shape node)
                  ::subtrees [(-> node
                                (update ::path conj 0)
                                (assoc ::extent new-height
                                       ::lens new-lens))
                              (-> node
                                (update ::path conj 1)
                                (dissoc ::focused)
                                (dissoc ::extent))]})
               node))))))

(s/fdef split-pane
  :args (s/cat :editor ::editor
               :new-lens ::lens)
  :ret ::split)
(defn split-pane
  [editor new-lens]
  (+> editor
    (split-pane' new-lens)
    simplify-panes
    resize-panes))

(defn- reachable
  [[i j] [di dj]]
  (fn [{[[pi pj] [plines pcols]] :avi.layout/shape}]
    (and (<= pj j (dec (+ pj pcols)))
         (if (pos? di)
           (< i pi)
           (< (dec (+ pi plines)) i)))))

(defn- distance
  [[i j] [di dj] {[[pi pj] [plines pcols]] :avi.layout/shape}]
  (reduce min (concat
                (if-not (zero? di)
                  [(Math/abs (- i pi))
                   (Math/abs (- i (dec (+ pi plines))))])
                (if-not (zero? dj)
                  [(Math/abs (- j pj))
                   (Math/abs (- j (dec (+ pj pcols))))]))))

(defn move
  [editor [di dj]]
  (let [[i j] (point-position editor)
        [_ pane] (transduce
                   (comp all-panes
                         (filter (reachable [i j] [di dj]))
                         (map (juxt #(distance [i j] [di dj] %) identity)))
                   (completing
                     (fn [[rd result :as a] [id input :as b]]
                       (if (< rd id) a b)))
                   [Long/MAX_VALUE nil]
                   (augmented-root-panes editor))]
    (if pane
      (assoc editor ::path (::path pane))
      (b/beep editor))))
