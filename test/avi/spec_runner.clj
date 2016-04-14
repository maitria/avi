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

(defn everything-after
  [comparison source-data]
  (->> source-data
    (drop-while (complement comparison))
    (second)))

(defn arrange
  []
  (->> (spec-lines)
    (map s/trim)
    (everything-after #(s/ends-with? % ":"))))

(defn action-line
  []
  (->> (spec-lines)
    (map s/trim)
    (everything-after #(= % (arrange)))))

(defn action
  []
  (second (re-find #"`(.*)`" (action-line))))

(defn matches-specs?
  []
  false)

(pending-fact "Avi matches the specs" 
  (matches-specs?) => truthy)
