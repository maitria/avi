(ns avi.layout
  (:require [avi.layout.panes :as p]
            [clojure.spec :as s]))

(s/def ::nat (s/and int? (complement neg?)))
(s/def ::shape (s/tuple (s/tuple ::nat ::nat)
                        (s/tuple ::nat ::nat)))

(def all-shapes
  (comp (mapcat p/augmented-root-panes)
        p/all-panes))
  
