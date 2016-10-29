(ns avi.document
  (:require [clojure.spec :as s]))

(s/def ::name string?)
(s/def ::text string?)
(s/def ::document (s/keys :req [::name ::text]))
