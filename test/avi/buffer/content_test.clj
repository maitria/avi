(ns avi.buffer.content-test
  (:require [avi.buffer.content :as c]
            [clojure.string :as string]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as gen']
            [com.gfredericks.test.chuck.properties :as prop']
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
    (:lines (c/content "foo\n")) => ["foo"])
  (fact "content starts at revision zero"
    (:revision (c/content "Hello!")) => 0)
  (fact "content starts with no history steps"
    (:history (c/content "Wha?!")) => {}))

(facts "about replacing contents"
  (fact "replace can insert at beginning of buffer"
    (:lines (c/replace (c/content "Hello!") [1 0] [1 0] "xyz")) => ["xyzHello!"])
  (fact "replace can insert within a line"
    (:lines (c/replace (c/content "Hello!") [1 2] [1 2] "//")) => ["He//llo!"])
  (fact "replace can insert at the end of a line"
    (:lines (c/replace (c/content "Hello!") [1 6] [1 6] "//")) => ["Hello!//"])
  (fact "replace increments contents revision"
    (:revision (c/replace (c/content "Hello!") [1 3] [1 3] "?")) => 1)
  (fact "replace records history steps"
    (:history (c/replace (c/content "Hello!") [1 2] [1 3] "??!!\nfy")) =>
      {0 {:start [1 2] :end [1 3] :+lines 1 :+columns 2}}))

(facts "about versioning marks"
  (fact "versioning marks adds the buffer revision"
    (c/versioned-mark (c/content "Hello!") [1 3]) => [1 3 0]))

(def text-generator
  (gen/fmap (partial string/join "\n") (gen/vector gen/string-ascii)))

(def content-generator
  (gen/fmap c/content text-generator))

(defn mark-generator
  [{:keys [lines]}]
  (gen'/for [line (gen/choose 1 (count lines))
             column (gen/choose 0 (count (get lines (dec line))))]
    [line column]))

(defn start-end-mark-generator
  [content]
  (gen/fmap sort (gen/vector (mark-generator content) 2)))

(defspec join-before-and-after-invariant 25
  (prop'/for-all [content content-generator
                  mark (mark-generator content)]
    (= (c/join (c/before (:lines content) mark) (c/after (:lines content) mark))
       (:lines content))))

(defspec replace-before-after-replacement-invariant 25
  (prop'/for-all [content content-generator
                  [start end] (start-end-mark-generator content)
                  replacement text-generator]
    (= (str (string/join "\n" (c/before (:lines content) start))
            replacement
            (string/join "\n" (c/after (:lines content) end)))
       (string/join "\n" (:lines (c/replace content start end replacement))))))

(facts "about unversioning marks"
  (fact "simple marks are passed through"
    (c/unversion (c/content "Hello!") [1 2]) => [1 2]))
