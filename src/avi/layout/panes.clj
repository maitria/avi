(ns avi.layout.panes
  (:require [avi.beep :as b]
            [clojure.spec :as s]))

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

(def annotate-renderable-type
  (map #(cond-> %
          (::lens %)
          (assoc :avi.layout/renderable-type :avi.layout.panes/pane))))

(defn- with-renderable-type
  [xform]
  (comp
    annotate-renderable-type
    xform
    (map #(dissoc % :avi.layout/renderable-type))))

(defn annotate
  "A transducer which annotates inputs by stateful process.

  f accepts state and input and returns [state' input'].  To index inputs:

     (annotate (fn [input n] [(assoc input ::pos n) (inc n)]) 0)"
  [f init]
  (fn [rf]
    (let [state (volatile! init)]
      (fn annotate-rf
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [[state' input'] (f @state input)]
           (vreset! state state')
           (rf result input')))
        ([result input & inputs]
         (let [[state' input' & inputs'] (apply f @state input inputs)]
           (vreset! state state')
           (apply rf result input' inputs')))))))

(defn- annotate-path
  [parent]
  (annotate (fn [state input]
              [(inc state) (assoc input ::path (conj (::path parent) state))])
            0))

(defn- with-path
  [xform parent]
  (comp
    (annotate-path parent)
    xform
    (map #(dissoc % ::path))))

(defn- annotate-shape
  [parent]
  (annotate (fn [[[i j] [rows cols]] input]
                  (let [height (or (::extent input) rows)]
                    [[[(+ i height) j] [(- rows height) cols]]
                     (assoc input :avi.layout/shape [[i j] [height cols]])]))
                (:avi.layout/shape parent)))

(defn- with-shape
  [xform parent]
  (comp
    (annotate-shape parent)
    xform
    (map #(dissoc % :avi.layout/shape))))

(defn- xfmap
  [xform tree]
  (cond-> tree
    (::subtrees tree)
    (update ::subtrees #(into [] (-> xform
                                   (with-shape tree)
                                   (with-path tree)
                                   with-renderable-type) %))))

(defn cata
  "A sort of more general catamorphism for transducers."
  [xfmap xform root]
  (letfn [(cata' [tree]
            (xfmap (comp (map cata') xform) tree))]
    (into []
          (comp (map cata') xform)
          [root])))

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
     (-> (transduce
           (comp
             (annotate-shape input)
             (annotate-path input)
             annotate-renderable-type
             all-nodes-rf)
           rf
           result
           (::subtrees input))
       (rf input)))))

(def all-panes (comp all-nodes (filter ::lens)))

(defn augmented-root-panes
  [{:keys [::tree] :as editor}]
  [(assoc tree
          :avi.layout/shape (root-pane-shape editor)
          ::path [])])

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

(s/fdef split-pane
  :args (s/cat :editor ::editor
               :new-lens ::lens)
  :ret ::split)
(defn split-pane
  [{:keys [::tree] :as editor} new-lens]
  (let [[_ [lines _]] (root-pane-shape editor)
        panes (if (::lens tree)
                [tree]
                (::subtrees tree))
        each-pane-height (int (/ (dec lines) (inc (count panes))))
        panes (-> (mapv #(assoc % ::extent each-pane-height) panes)
                (into [{::lens new-lens}]))]
    (assoc editor
           ::tree {::subtrees panes}
           ::path [0])))

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
