(ns avi.lenses
 (:require [clojure.spec :as s]))

(s/def ::document :avi.documents/document-ref)
(s/def ::viewport-top nat-int?)
(s/def ::point (s/tuple nat-int? nat-int?))
(s/def ::last-explicit-j nat-int?)

(s/def ::lens (s/keys :req [::document
                            ::viewport-top
                            ::point
                            ::last-explicit-j]))

(s/def ::lenses (s/map-of nat-int? ::lens))
(s/def ::editor (s/keys :req [::lenses]))
