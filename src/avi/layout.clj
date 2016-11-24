(ns avi.layout
  (:require [avi.layout.panes :as p]
            [clojure.spec :as s]))

(s/def ::position (s/tuple nat-int? nat-int?))
(s/def ::width nat-int?)
(s/def ::text string?)
(s/def ::foreground :avi.color/color)
(s/def ::background :avi.color/color)

(s/def ::shape (s/tuple ::position
                        (s/tuple nat-int? nat-int?)))
(s/def ::renderable-type keyword?)

(def all-renderables
  (comp (mapcat p/augmented-root-panes)
        p/all-renderables))
  
(defmulti blits
  (fn [rendition renderable editor rf]
    (::renderable-type renderable)))
