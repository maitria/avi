(ns avi.documents
 (:refer-clojure :exclude [load])
 (:require [clojure.spec :as s]
           [avi.edit-context
            [lines :as lines]]
           [avi.world :as w])
 (:import (java.io FileNotFoundException)))

(s/def ::name string?)
(s/def ::text string?)
(s/def ::lines (s/coll-of string? :into vector?))
(s/def ::in-transaction? boolean?)
(s/def ::undo-log (s/coll-of any? :into list?))
(s/def ::redo-log (s/coll-of any? :into list?))

(s/def ::document (s/keys :req [::name
                                ::text
                                ::lines
                                ::in-transaction?
                                ::undo-log
                                ::redo-log]))
(s/def ::documents (s/map-of nat-int? ::document))
(s/def ::document-ref nat-int?)

(s/def ::editor (s/keys :req [::documents]))


(defn- try-load
  [filename]
  (try
    (w/read-file w/*world* filename)
    (catch FileNotFoundException e
     "")))

(defn load
  [filename]
  (let [text (if filename
               (try-load filename)
               "")]
    {::name filename
     ::text text
     ::lines (lines/content text)
     ::undo-log ()
     ::redo-log ()
     ::in-transaction? false}))
