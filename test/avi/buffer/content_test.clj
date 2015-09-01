(ns avi.buffer.content-test
  (:require [avi.buffer.content :as c]
            [clojure.string :as string]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as gen']
            [midje.sweet :refer :all]
            [schema.core :as s]))

(s/set-fn-validation! true)

(facts "about buffer contents"
  (fact "we can retrieve buffer contents initial text"
    (:lines (c/content "Hello, World!")) => ["Hello, World!"]
    (:lines (c/content "Line 1\nLine 2")) => ["Line 1" "Line 2"]
    (:lines (c/content "Line 1\nLine 3\n")) => ["Line 1" "Line 3"]
    (:lines (c/content "Line 1\n\nLine 3")) => ["Line 1" "" "Line 3"])
  (fact "we always have at least one line"
    (:lines (c/content "")) => [""])
  (fact "if the last character is a newline, it does not make an extra line"
    (:lines (c/content "\n")) => [""]
    (:lines (c/content "\n\n")) => ["" ""]
    (:lines (c/content "\nfoo")) => ["" "foo"]
    (:lines (c/content "foo\n")) => ["foo"]))

(facts "about replacing contents"
  (fact "replace can insert at beginning of buffer"
    (:lines (c/replace (c/content "Hello!") [1 0] [1 0] "xyz")) => ["xyzHello!"])
  (fact "replace can insert within a line"
    (:lines (c/replace (c/content "Hello!") [1 2] [1 2] "//")) => ["He//llo!"])
  (fact "replace can insert at the end of a line"
    (:lines (c/replace (c/content "Hello!") [1 6] [1 6] "//")) => ["Hello!//"]))

(def replace-generator
  (gen'/for [initial-text (gen/fmap (partial string/join "\n") (gen/vector gen/string-ascii))
             :let [content (c/content initial-text)
                   line-count (count (:lines content))]
             start-line (gen/choose 1 line-count)
             end-line (gen/choose start-line line-count)
             start-column (gen/choose 0 (count (get-in content [:lines (dec start-line)])))
             end-column (if (= start-line end-line)
                          (gen/choose start-column (count (get-in content [:lines (dec end-line)])))
                          (gen/choose 0 (count (get-in content [:lines (dec end-line)]))))
             replacement gen/string-ascii]
    {:replacement replacement
     :start [start-line start-column]
     :end [end-line end-column]
     :pre-content content
     :post-content (c/replace content [start-line start-column] [end-line end-column] replacement)}))

(defspec replace-does-not-change-lines-prior-to-first-mark 25
  (prop/for-all [{:keys [pre-content post-content] [start-line] :start :as foo} replace-generator]
        
    (every?
      #(= (get-in pre-content [:lines (dec %)]) (get-in post-content [:lines (dec %)]))
      (range 1 start-line))))
