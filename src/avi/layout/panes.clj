(ns avi.layout.panes
  (:require [avi.beep :as b]
            [avi.xforms :as xf]
            [clojure.spec :as s]
            [packthread.core :refer :all]))

;; Yeah, so panes in a horizontal split are layed out vertically, and vice
;; versa

(s/def ::lens nat-int?)
(s/def ::extent nat-int?)
(s/def ::direction #{:horizontal :vertical})
(s/def ::subtrees (s/coll-of ::tree :type vector?))
(s/def ::pane (s/keys :req [::lens]))
(s/def ::split (s/keys :req [::direction ::subtrees]))
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
  {:pre [(:avi.layout/shape parent)]}
  (case (::direction parent)
    :horizontal
    (xf/annotate (fn [[[i j] [rows cols]] input]
                   (let [height (or (::extent input) rows)]
                     [[[(+ i height) j] [(- rows height) cols]]
                      (assoc input :avi.layout/shape [[i j] [height cols]])]))
                 (:avi.layout/shape parent))

    :vertical
    (xf/annotate (fn [[[i j] [rows cols]] input]
                   (let [width (or (::extent input) cols)]
                     [[[i (+ j width 1)] [rows (- cols width 1)]]
                      (assoc input :avi.layout/shape [[i j] [rows width]])]))
                 (:avi.layout/shape parent))

    identity))

(defn- annotate-path
  [parent]
  (xf/annotate (fn [state input]
                 [(inc state) (assoc input ::path (conj (::path parent) state))])
               0))

(defn- add-renderable-type
  [tree]
  (if (::lens tree)
    (assoc tree :avi.layout/renderable-type ::pane)
    tree))

(def annotate-renderable-type
  (map add-renderable-type))

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

(def all-renderables
  (comp all-panes
        (fn [rf]
          (fn
            ([] (rf))
            ([result] (rf result))
            ([result input]
             (let [[[i j] [rows cols]] (:avi.layout/shape input)
                   result (rf result input)]
               (cond-> result
                 (not (zero? j))
                 (rf {:avi.layout/shape [[i (dec j)] [rows 1]]
                      :avi.layout/renderable-type ::vertical-bar}))))))))

(defn- augmented-root-pane
  [{:keys [::tree] :as editor}]
  (+> tree
    add-renderable-type
    (assoc :avi.layout/shape (root-pane-shape editor)
           ::path [])))

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
  (let [{:keys [::lens] [[pi pj] _] :avi.layout/shape :as pane}
          (current-pane editor)
        {:keys [viewport-top] [i j] :point} (get-in editor [:lenses lens])]
    [(+ (- i viewport-top) pi)
     (+ pj j)]))

(defn pane-tree-cata
  [editor xform]
  {:pre [(::lens (current-pane editor))]}
  (let [tree (augmented-root-pane editor)
        tree (first (xf/cata
                      xfmap
                      (comp
                        (map #(cond-> %
                                (= (::path editor) (::path %))
                                (assoc ::focused? true)))
                        xform)
                      tree))
        tree (assoc tree ;; hack! see augmented-root-pane
                    :avi.layout/shape (root-pane-shape editor)
                    ::path [])
        path (::path (first (sequence
                              (comp all-nodes (filter ::focused?))
                              [tree])))
        _ (assert path)
        tree (first (xf/cata xfmap (map #(dissoc % ::focused?)) tree))]
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
      (let [{:keys [::direction], [[i j] [rows cols]] :avi.layout/shape} tree
            extent (case direction
                     :horizontal
                     (int (/ (dec rows) (count subtrees)))
                     :vertical
                     (dec (int (/ cols (count subtrees)))))]
        (assoc ::subtrees
               (into []
                     (map #(assoc % ::extent extent))
                     subtrees))))))

(defn resize-panes
  "Makes all panes equal size."
  [editor]
  (pane-tree-cata editor (comp (map assign-equal-extents)
                               (map remove-last-child-extent))))

(defn- flatten-like-splits
  [tree]
  (+> tree
   (if (::subtrees tree)
     (update ::subtrees
             (fn [subtrees]
               (into []
                     (mapcat (fn [child]
                               (if (or (::lens child)
                                       (not= (::direction tree) (::direction child)))
                                 [child]
                                 (::subtrees child))))
                     subtrees))))))

(defn- remove-one-child-splits
  [tree]
  (+> tree
    (if (= 1 (count (::subtrees tree)))
      (get-in [::subtrees 0]))))

(defn- simplify-panes
  [editor]
  (pane-tree-cata editor (comp (map remove-one-child-splits)
                               (map flatten-like-splits))))

(defn- split-pane'
  [editor new-lens direction]
  (+> editor
    (pane-tree-cata
      (map (fn [node]
             (if (::focused? node)
               (let [[_ [rows cols]] (:avi.layout/shape node)
                     old-extent (case direction
                                  :horizontal rows
                                  :vertical cols)
                     new-extent (max 2 (int (/ old-extent 2)))]
                 {::direction direction
                  ::path (::path node)
                  :avi.layout/shape (:avi.layout/shape node)
                  ::subtrees [(-> node
                                (update ::path conj 0)
                                (assoc ::extent new-extent
                                       ::lens new-lens))
                              (-> node
                                (update ::path conj 1)
                                (dissoc ::focused?)
                                (dissoc ::extent))]})
               node))))))

(s/fdef split-pane
  :args (s/cat :editor ::editor
               :new-lens ::lens
               :direction ::direction)
  :ret ::split)
(defn split-pane
  [editor new-lens direction]
  {:post [(::lens (current-pane %))]}
  (let [{:keys [::lens] [[_] [rows cols]] :avi.layout/shape :as pane}
          (current-pane editor)]
    (if (or (and (= direction :horizontal) (> (/ rows 2) 2))
           (and (= direction :vertical)  (> (/ cols 2) 2)))
      (+> editor
        (split-pane' new-lens direction)
        simplify-panes
        resize-panes)
      (b/beep editor "No room for new Pane"))))

(defn- focused-path-after-close
  [{:keys [::path] :as editor}]
  (let [prefix (conj (pop path) (if (zero? (peek path))
                                  1
                                  (dec (peek path))))]
    (first (sequence
             (comp
               all-panes
               (map ::path)
               (filter (fn [p]
                         (and (<= (count prefix) (count p))
                              (= prefix (subvec p 0 (count prefix)))))))
             [(augmented-root-pane editor)]))))

(defn close-pane
  [{:keys [::tree ::path] :as editor}]
  {:pre [(::lens (current-pane editor))]
   :post [(or (:finished? %) (::lens (current-pane %)))]}
  (+> editor
    (if (::lens tree)
      (assoc :finished? true)
      (let [new-path (focused-path-after-close editor)]
        (pane-tree-cata
          (comp (remove #(= (::path %) path))
                (map (fn [pane]
                       (cond-> pane
                         (= (::path pane) new-path)
                         (assoc ::focused? true))))))
        simplify-panes
        resize-panes))))

(defn- reachable
  [[i j] [di dj]]
  {:pre [(or (zero? di) (zero? dj))]}
  (fn [{[[pi pj] [plines pcols]] :avi.layout/shape}]
    (if (zero? dj)
      (and (<= pj j (dec (+ pj pcols)))
           (if (pos? di)
             (< i pi)
             (< (dec (+ pi plines)) i)))
      (and (<= pi i (dec (+ pi plines)))
           (if (pos? dj)
             (< j pj)
             (< (dec (+ pj pcols)) j))))))

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
