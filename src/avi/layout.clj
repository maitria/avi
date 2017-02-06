(ns avi.layout
 (:require [avi.color]
           [avi.layout.panes :as p]
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

(defmulti blit-producer
  "To keep implementation open, register unique 'blit producers' on this
  multi-method.  The dispatch fn is fake and the dispatch key is not
  used; use a unique value for each producer so that code will reload
  properly when developing.

  Takes a reducing function and returns a reducing function (it is
  expected to apply a transducer to it).  The input reducing function
  takes :avi.layout/blit objects to render, the output is expected to
  process whole :avi.editor/editor objects."
  (fn [rf]
    (throw (Exception. "Do not call directly!"))))

(defn all-blits
  [rf]
  (let [;; wrap-rf is used to ensure rf is only initialized once and
        ;; terminated once.
        wrap-rf (fn ([] nil)
                    ([result] result)
                    ([result input] (rf result input)))
        producer-rfs (map #(% wrap-rf) (vals (methods blit-producer)))]
    (fn ([]
         (doseq [prf producer-rfs]
           (prf))
         (rf))
        ([result]
         (rf (reduce #(%2 %1) result producer-rfs)))
        ([result input]
         (reduce #(%2 %1 input) result producer-rfs)))))
