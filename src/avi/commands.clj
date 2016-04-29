(ns avi.commands
  "Avi's command-line-mode commands.
  
  Functions implemented in this namespace can be called by name from the colon
  prompt."
  (:require [avi.edit-context :as ec]
            [avi.editor :as e]
            [avi.world :as w]
            [clojure.string :as string]
            [packthread.core :refer :all]))

(defn -NUMBER-
  "Special function which handles commands like `:42`"
  [editor command-line]
  (+> editor
    (in e/edit-context
        (ec/operate {:operator :move-point
                     :motion [:goto [(dec (Long/parseLong command-line)) :first-non-blank]]}))))

(defn q
  [editor]
  (assoc editor :finished? true))

(defn w
  [editor]
  (let [{filename :name, :keys [lines]} (get-in editor (e/current-document-path editor))]
    (w/write-file w/*world* filename (string/join "\n" lines))
    editor))

(def wq (comp q w))

(defn sp
  [{:keys [lenses] {[lines columns] :size} :viewport :as editor}]
  (let [first-height (int (/ (dec lines) 2))
        new-lenses (conj lenses (e/current-lens editor))]
    (+> editor
      (assoc :lenses new-lenses
             :panes [:h 0 first-height 1]
             :pane-path [0]))))
