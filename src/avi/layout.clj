(ns avi.layout
  (:require [avi.layout.panes :as p]
            [clojure.spec :as s]))

(s/def ::shape (s/tuple (s/tuple nat-int? nat-int?)
                        (s/tuple nat-int? nat-int?)))
(s/def ::renderable-type keyword?)

(def all-renderables
  (comp (mapcat p/augmented-root-panes)
        p/all-renderables))
  
(defmulti render!
  (fn [editor rendition renderable]
    (::renderable-type renderable)))

(defmethod render! :default
  [editor rendition renderable]
  nil)

(defmulti blits
  (fn [rendition renderable editor rf]
    (::renderable-type renderable)))

(defmethod blits :default
  [rendition _ _ _]
  rendition)
