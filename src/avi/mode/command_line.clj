(ns avi.mode.command-line
  (:require [packthread.core :refer :all]
            [avi.edit-context :as ec]
            [avi.command-line :as cl]
            [avi.commands]
            [avi.editor :as e]
            [avi.pervasive :refer :all])
  (:import (java.io StringReader PushbackReader)))

(def normal-commands
  {":" (fn+> [editor _]
         (cl/enter :command-line ":"))})

(defn- line-number?
  [command]
  (every? #(Character/isDigit %) command))

(defmulti type-hinted-read
  "To emulate the Vim command-line, our commands can hint us about the type
  of their arguments."
  (fn [hint rdr]
    hint))

(defmethod type-hinted-read nil
  [hint rdr]
  (read {:eof ::eof} rdr))
(defmethod type-hinted-read ::string
  [hint rdr]
  (let [token (read {:eof ::eof} rdr)]
    (cond-> token
      (not= token ::eof)
      str)))

(defn- resolve-head
  [x]
  (cond
    (= ::eof x)
    [:ok (list #'identity)]

    (symbol? x)
    (if-some [v (ns-resolve 'avi.commands x)]
      [:ok (list v)]
      [:fail (str ":" x " is not a thing")])

    (number? x)
    [:ok (list #'avi.commands/-NUMBER- x)]))

(defn- parse
  [command-line]
  (with-open [rdr (PushbackReader. (StringReader. command-line))]
    (when-some [[ok head :as result] (resolve-head (read {:eof ::eof} rdr))]
      (if-not (= ok :ok)
        result
        [:ok (concat head
                     (doall (->> (:type-hints (meta (first head)))
                                 (map #(type-hinted-read % rdr))
                                 (take-while (complement #{::eof})))))]))))

(defn- process-command
  [editor command-line]
  (let [[ok form] (parse command-line)]
    (if (= ok :ok)
      (apply (first form) editor (rest form))
      (assoc editor :message [:white :red (str ":" command-line " is not a thing")]))))

(def wrap-mode (cl/mode-middleware :command-line process-command))
