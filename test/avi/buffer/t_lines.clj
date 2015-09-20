(ns avi.buffer.t-lines
  (:require [avi.buffer.lines :as lines]
            [clojure.string :as string]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as gen']
            [com.gfredericks.test.chuck.properties :as prop']
            [midje.sweet :refer :all]
            [midje.checking.core :as checking]
            [schema.core :as s]))

(s/set-fn-validation! true)

(defn check-inside
  [& path]
  (fn [& args]
    (fn [outer-value]
      (let [expected-value (last args)
            more-path (drop-last args)
            value (get-in outer-value (concat path more-path))]
        (checking/extended-= value expected-value)))))

(def lines (check-inside :lines))

(facts "about buffer contents"
  (fact "we can retrieve buffer contents' initial text"
    (lines/content "Hello, World!") => (lines ["Hello, World!"])
    (lines/content "Line 1\nLine 2") => (lines ["Line 1" "Line 2"])
    (lines/content "Line 1\nLine 3\n") => (lines ["Line 1" "Line 3"])
    (lines/content "Line 1\n\nLine 3") => (lines ["Line 1" "" "Line 3"]))
  (fact "we always have at least one line"
    (lines/content "") => (lines [""]))
  (fact "if the last character is a newline, it does not make an extra line"
    (lines/content "\n") => (lines [""])
    (lines/content "\n\n") => (lines ["" ""])
    (lines/content "\nfoo") => (lines ["" "foo"])
    (lines/content "foo\n") => (lines ["foo"])))

(facts "about replacing contents"
  (fact "replace can insert at beginning of buffer"
    (lines/replace (lines/content "Hello!") [1 0] [1 0] "xyz") => (lines ["xyzHello!"]))
  (fact "replace can insert within a line"
    (lines/replace (lines/content "Hello!") [1 2] [1 2] "//") => (lines ["He//llo!"]))
  (fact "replace can insert at the end of a line"
    (lines/replace (lines/content "Hello!") [1 6] [1 6] "//") => (lines ["Hello!//"])))

(def text-generator
  (gen/fmap (partial string/join "\n") (gen/vector gen/string-ascii)))

(def content-generator
  (gen/fmap lines/content text-generator))

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
    (= (lines/join (lines/before (:lines content) mark) (lines/after (:lines content) mark))
       (:lines content))))

(defspec replace-before-after-replacement-invariant 25
  (prop'/for-all [content content-generator
                  [start end] (start-end-mark-generator content)
                  replacement text-generator]
    (= (str (string/join "\n" (lines/before (:lines content) start))
            replacement
            (string/join "\n" (lines/after (:lines content) end)))
       (string/join "\n" (:lines (lines/replace content start end replacement))))))
