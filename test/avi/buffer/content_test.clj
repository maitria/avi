(ns avi.buffer.content-test
  (:require [avi.buffer.content :as c]
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
  (fn [expected-value]
    (fn [outer-value]
      (let [value (get-in outer-value path)]
      (checking/extended-= value expected-value)))))

(def lines (check-inside :lines))
(def revision (check-inside :revision))

(defn history
  ([expected-history]
   (fn [{:keys [history]}]
     (checking/extended-= history expected-history)))
  ([n expected-item]
   (fn [{:keys [history]}]
     (checking/extended-= (get history n) expected-item))))

(facts "about buffer contents"
  (fact "we can retrieve buffer contents initial text"
    (c/content "Hello, World!") => (lines ["Hello, World!"])
    (c/content "Line 1\nLine 2") => (lines ["Line 1" "Line 2"])
    (c/content "Line 1\nLine 3\n") => (lines ["Line 1" "Line 3"])
    (c/content "Line 1\n\nLine 3") => (lines ["Line 1" "" "Line 3"]))
  (fact "we always have at least one line"
    (c/content "") => (lines [""]))
  (fact "if the last character is a newline, it does not make an extra line"
    (c/content "\n") => (lines [""])
    (c/content "\n\n") => (lines ["" ""])
    (c/content "\nfoo") => (lines ["" "foo"])
    (c/content "foo\n") => (lines ["foo"]))
  (fact "content starts at revision zero"
    (c/content "Hello!") => (revision 0))
  (fact "content starts with no history steps"
    (c/content "Wha?!") => (history {})))

(facts "about replacing contents"
  (fact "replace can insert at beginning of buffer"
    (c/replace (c/content "Hello!") [1 0] [1 0] "xyz") => (lines ["xyzHello!"]))
  (fact "replace can insert within a line"
    (c/replace (c/content "Hello!") [1 2] [1 2] "//") => (lines ["He//llo!"]))
  (fact "replace can insert at the end of a line"
    (c/replace (c/content "Hello!") [1 6] [1 6] "//") => (lines ["Hello!//"]))
  (fact "replace increments contents revision"
    (c/replace (c/content "Hello!") [1 3] [1 3] "?") => (revision 1))
  (fact "replace records history steps"
    (c/replace (c/content "Hello!") [1 2] [1 3] "??!!\nfy") =>
      (history 0 {:start [1 2] :end [1 3] :+lines 1 :+columns 2}))
  (fact "replace can use versioned marks"
    (-> (c/content "Hello!")
      (c/replace [1 2] [1 2] "xxx")
      (c/replace [1 1 0] [1 3 0] "yyy")) => (lines ["Hyyylo!"])))

(facts "about versioning marks"
  (fact "versioning marks adds the buffer revision"
    (c/version-mark (c/content "Hello!") [1 3]) => [1 3 0]))

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
  (fact "unversioning passes simple marks through"
    (c/unversion-mark (c/content "Hello!") [1 2]) => [1 2])
  (fact "unversioning a mark with the current revision just discards the version"
    (c/unversion-mark (c/content "Hello!") [1 2 0]) => [1 2])
  (fact "unversioning a mark that was before any changes doesn't change the position"
    (let [content (c/content "Hello!")
          old-mark (c/version-mark content [1 2])
          new-content (c/replace content [1 3] [1 3] "xxx")]
      (c/unversion-mark new-content old-mark) => [1 2]))
  (fact "unversioning a mark >1 lines after any changes moves down"
    (let [content (c/content "Hello!\nWorld")
          old-mark (c/version-mark content [2 2])
          new-content (c/replace content [1 3] [1 3] "x\n\nxx")]
      (c/unversion-mark new-content old-mark) => [4 2]))
  (fact "unversioning a mark on same line as end mark, but to the right, moves right"
    (let [content (c/content "Hello!\nWorld")
          old-mark (c/version-mark content [2 2])
          new-content (c/replace content [2 0] [2 0] "123")]
      (c/unversion-mark new-content old-mark) => [2 5]))
  (fact "unversioning a mark for a replaced region returns nil"
    (let [content (c/content "Hello!")
          old-mark (c/version-mark content [1 3])
          new-content (c/replace content [1 2] [1 4] "123")]
      (c/unversion-mark new-content old-mark) => nil)))
