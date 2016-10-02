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
