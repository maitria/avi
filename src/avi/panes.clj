(ns avi.panes
  (:require [clojure.spec :as s]))

(s/def ::nat (s/and int? (complement neg?)))
(s/def ::direction #{:h :v})
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
      :offset [i j]
      :size [lines columns]}]
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
          (update-in tree ::subtrees (comp vec rest))
          (into [(dec phead)] prest)
          [[(+ i extent) j]
           [(- rows extent) cols]])))
    shape))

(s/fdef internal-pane-height
  :args (s/cat :panes ::tree
               :slot ::nat
               :outer-pane-height ::nat)
  :ret ::nat)
(defn- internal-pane-height
  [panes pane-index outer-pane-height]
  (if (= (inc pane-index) (count (::subtrees panes)))
    (- outer-pane-height
       (->> (::subtrees panes)
         (keep ::extent)
         (reduce +)))
    (get-in panes [::subtrees pane-index ::extent])))

(defn current-pane-height
  [{:keys [::tree ::path] :as editor}]
  (let [[rows cols] (get-in editor [:viewport :size])
        [_ [rows _]] (shape tree path [[0 0] [(dec rows) cols]])]
    rows))

(defn internal-pane-top
  [{:keys [::subtrees]} pane-number outer-pane-top]
  (+ outer-pane-top
     (->> subtrees
       (keep ::extent)
       (take pane-number)
       (reduce +))))

(s/fdef top
  :args (s/cat :panes ::tree
               :path ::path
               :pane-height ::nat
               :pane-top ::nat)
  :ret ::nat)
(defn top
  [panes path pane-height pane-top]
  (if (empty? path)
    pane-top
    (recur
      (get-in panes [::subtrees (first path)])
      (rest path)
      (internal-pane-height panes (first path) pane-height)
      (internal-pane-top panes (first path) pane-top))))

(defn current-pane-top
  [{:keys [::tree ::path] :as editor}]
  (top tree path (dec (get-in editor [:viewport :size 0])) 0))

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
