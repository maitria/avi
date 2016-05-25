(ns avi.edit-context.t-lines
  (:require [avi.edit-context.lines :as lines]
            [clojure.string :as string]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as gen']
            [com.gfredericks.test.chuck.properties :as prop']
            [midje.sweet :refer :all]
            [midje.checking.core :as checking]
            [schema.core :as s]
            [avi.test-helpers :refer :all]))

(facts "about buffer contents"
  (fact "we can retrieve buffer contents' initial text"
    (lines/content "Hello, World!") => ["Hello, World!"]
    (lines/content "Line 1\nLine 2") => ["Line 1" "Line 2"]
    (lines/content "Line 1\nLine 3\n") => ["Line 1" "Line 3"]
    (lines/content "Line 1\n\nLine 3") => ["Line 1" "" "Line 3"])
  (fact "we always have at least one line"
    (lines/content "") => [""])
  (fact "if the last character is a newline, it does not make an extra line"
    (lines/content "\n") => [""]
    (lines/content "\n\n") => ["" ""]
    (lines/content "\nfoo") => ["" "foo"]
    (lines/content "foo\n") => ["foo"]))

(def text-generator
  (gen/fmap (partial string/join "\n") (gen/vector gen/string-ascii)))

(def content-generator
  (gen/fmap lines/content text-generator))

(defn location-generator
  [lines extra-columns]
  (gen'/for [:parallel [line (gen/choose 0 (dec (count lines)))
                        column (gen/choose 0 (+ extra-columns (count (get lines line))))]]
    [line column]))

(defn start-end-location-generator
  [lines]
  (gen/fmap sort (gen/vector (location-generator lines 10) 2)))

(facts "about replacing contents"
  (fact "replace can insert at beginning of buffer"
    (lines/replace (lines/content "Hello!") [0 0] [0 0] "xyz") => ["xyzHello!"])
  (fact "replace can insert within a line"
    (lines/replace (lines/content "Hello!") [0 2] [0 2] "//") => ["He//llo!"])
  (fact "replace can insert at the end of a line"
    (lines/replace (lines/content "Hello!") [0 6] [0 6] "//") => ["Hello!//"])
  (fact "replace works with start and end reversed"
    (lines/replace (lines/content "Hello!") [0 5] [0 2] "//") => ["He//!"])
  (fact "replace works after end-of-line by inserting spaces"
    (lines/replace ["x"] [0 3] [0 4] "!") => ["x  !"])
  (fact "replace can insert new lines"
    (lines/replace ["hello" "world"] [1 0] [1 0] "\n") => ["hello" "" "world"]
    (lines/replace ["hello"] [0 2] [0 3] "\n") => ["he" "lo"])
  (fact "replace can append newlines at end-of-line"
    (lines/replace (lines/content "xx") [0 2] [0 2] "\n") => ["xx" ""])
  (property "join before and after an arbitrary location in line results in original"
    (prop'/for-all [content content-generator
                    location (location-generator content 0)]
      (= (lines/join (lines/before content location) (lines/after content location))
         content))          )
  (property "replace = before + replacement + after invariant"
    (prop'/for-all [content content-generator
                    [start end] (start-end-location-generator content)
                    replacement text-generator]
      (= (str (string/join "\n" (lines/before content start))
              replacement
              (string/join "\n" (lines/after content end)))
         (string/join "\n" (lines/replace content start end replacement))))))
