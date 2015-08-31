(ns avi.buffer.content-test
  (:require [midje.sweet :refer :all]
            [schema.core :as s]
            [avi.buffer.content :as content]))

(s/set-fn-validation! true)

(facts "about buffer contents"
  (fact "we can retrieve its initial text"
    (:lines (content/content "Hello, World!")) => ["Hello, World!"]))
