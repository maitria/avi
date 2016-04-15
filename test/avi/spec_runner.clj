(ns avi.spec-runner
  (:require [midje.sweet :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [avi.test-helpers :refer :all]))

(def ^:private tabular-quality-names
  '{editing ?editing
    after   ?after
    point   ?point})

(defn- qualities
  [spec]
  (take-while tabular-quality-names spec))

(defn- table-headings
  [spec]
  (map tabular-quality-names (qualities spec)))

(defn- table-data
  [spec]
  (drop-while tabular-quality-names spec))

(defn- editor-invocation
  [spec]
  (let [qualities (into #{} (qualities spec))]
    (cond-> `(editor)
      (qualities 'editing) (concat [:editing (tabular-quality-names 'editing)])
      (qualities 'after)   (concat [:after   (tabular-quality-names 'after)]))))

(defmacro facts-about
  [description & spec]
  `(tabular
     (facts ~description
       ~(editor-invocation spec) => (point ~'?point))
     ~@(table-headings spec)
     ~@(table-data spec)))

(defn spec-lines
  []
  (s/split-lines (slurp (io/resource "spec.txt"))))

(defn line-after
  [comparison]
  (->> (spec-lines)
    (map s/trim)
    (drop-while (complement comparison))
    (second)))

(declare arrange)

(defn claim-line?
  [line]
  (s/ends-with? line ":")) 

(defn arrange-line?
  [line]
  (= line (arrange)))

(def arrange (partial line-after claim-line?))
(def action-line (partial line-after arrange-line?))

(defn resulting-point
  []
  (->> (action-line)
    (re-find #"the point .. (\d+), (\d+)")
    (rest)
    (map read-string)
    (vec)))

(defn action
  []
  (second (re-find #"`(.*)`" (action-line))))

(defn test-parameters
  []
  {:setup {:after (action)
           :editing "hello world"}
   :point (resulting-point)})

(defn indent-level
  [node]
  (count (re-find #"^\s+" node)))

(defn string->node
  [words]
  [(s/triml words)])

(defn lines->tree
  [lines]
  [(concat (string->node (first lines))
    (if (next lines) 
      (lines->tree (rest lines))))]
      )

(defn specs
  [spec-lines]
  [])

(defn matches-specs?
  []
  false)

(pending-fact "Avi matches the specs" 
  (matches-specs?) => truthy)
