(ns avi.commands
  "Avi's command-line-mode commands.

  Functions implemented in this namespace can be called by name from the colon
  prompt."
  (:require [avi.document]
            [avi.edit-context :as ec]
            [avi.edit-context
              [lines :as lines]]
            [avi.editor :as e]
            [avi.layout.panes :as p]
            [avi.world :as w]
            [clojure.string :as string]
            [packthread.core :refer :all]))

(defn -NUMBER-
  "Special function which handles commands like `:42`"
  [editor n]
  (+> editor
    (in e/edit-context
        (ec/operate {:operator :move-point
                     :motion [:goto [(dec n) :first-non-blank]]}))))

(defn q
  [editor]
  (p/close-pane editor))

(defn w
  [editor]
  (let [{filename :avi.document/name,
         :keys [:avi.document/lines]} (get-in editor (e/current-document-path editor))]
    (w/write-file w/*world* filename (string/join "\n" lines))
    editor))

(def wq (comp q w))

(defn- split*
  [direction]
  (fn [{:keys [lenses] :as editor}]
    (-> editor
        (update :lenses assoc (inc (reduce max -1 (keys lenses))) (e/current-lens editor))
        (p/split-pane (count lenses) direction))))
(def sp (split* :horizontal))
(def vsp (split* :vertical))

(defn e
  {:type-hints [:avi.mode.command-line/string]}
  [editor filename]
  (+> editor
    (let [document-n (count (:avi.document/documents editor))]
      (update :avi.document/documents conj (avi.document/load filename))
      (assoc-in (conj (e/current-lens-path editor) :document) document-n))))
