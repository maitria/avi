(ns avi.document
  (:require [clojure.spec :as s]))

(s/def ::name string?)
(s/def ::text string?)
(s/def ::in-transaction? boolean?)
(s/def ::undo-log (s/coll-of any? :into list?))
(s/def ::redo-log (s/coll-of any? :into list?))

(s/def ::document (s/keys :req [::name
                                ::text
                                ::in-transaction?
                                ::undo-log
                                ::redo-log]))
