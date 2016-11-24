(ns avi.layout
  (:require [avi.layout.panes :as p]
            [clojure.spec :as s]))

;; A blit is a one atomic unit drawn to the text buffer.  For efficiency's
;; sake, it is a string, which cannot span lines, and all of the cells have
;; the same foreground and background color.
(s/def ::position (s/tuple nat-int? nat-int?))
(s/def ::width nat-int?)
(s/def ::text (s/and string? (complement #(re-find #"\n" %))))
(s/def ::foreground :avi.color/color)
(s/def ::background :avi.color/color)
(s/def ::blit (s/keys :req [::position
                            ::width
                            ::text
                            ::foreground
                            ::background]))

(s/def ::shape (s/tuple ::position
                        (s/tuple nat-int? nat-int?)))
(s/def ::renderable-type keyword?)

(def all-renderables
  (comp (mapcat p/augmented-root-panes)
        p/all-renderables))
  
(defmulti blits
  (fn [rendition renderable editor rf]
    (::renderable-type renderable)))
