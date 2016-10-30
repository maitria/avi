(ns avi.editor
  "Functions (including basse responders, middleware, and utilties) for
   manipulating the editor map."
  (:import (java.io FileNotFoundException))
  (:require [clojure.spec :as s]
            [clojure.set :as set]
            [packthread.core :refer :all]
            [packthread.lenses :as l]
            [avi.pervasive :refer :all]
            [avi.beep :as beep]
            [avi.document]
            [avi.edit-context :as ec]
            [avi.edit-context
              [lines :as lines]]
            [avi.layout :as layout]
            [avi.layout.panes :as p]
            [avi.world :as w]))

(s/def ::editor (s/merge ::p/editor))

;; -- Initial state ----------------------------------------------------------

(defn- try-load
  [filename]
  (try
    (w/read-file w/*world* filename)
    (catch FileNotFoundException e
     "")))

(defn document
  [filename]
  (let [text (if filename
               (try-load filename)
               "")]
    {:avi.document/name filename
     :avi.document/text text
     :avi.document/lines (lines/content text)
     :avi.document/undo-log ()
     :avi.document/redo-log ()
     :avi.document/in-transaction? false}))

(defn initial-editor
  [[lines columns] [filename]]
  {:mode :normal
   :documents [(document filename)]
   :lenses {0 {:document 0
               :viewport-top 0
               :point [0 0]
               :last-explicit-j 0}}
   ::p/tree {:avi.layout.panes/lens 0}
   ::p/path []
   ::layout/shape [[0 0] [lines columns]]
   :beep? false})

;; -- Building middlewares ---------------------------------------------------

(defn keystroke-middleware
  [keystroke a-fn]
  (fn [handler]
    (fn [editor event]
      (if (= event [:keystroke keystroke])
        (a-fn editor)
        (handler editor event)))))

;; -- Tracking the current lens & document -----------------------------------

(defn current-lens-path
  [editor]
  [:lenses (::p/lens (p/current-pane editor))])

(defn current-lens
  [editor]
  (get-in editor (current-lens-path editor)))

(defn current-document-path
  [editor]
  [:documents (:document (current-lens editor))])

(s/fdef edit-context
  :args (s/cat :editor ::editor
               :new-context (s/? any?))
  :ret ::editor)
(let [document-keys #{:avi.document/lines
                      :avi.document/undo-log
                      :avi.document/redo-log
                      :avi.document/in-transaction?}
      computed-keys #{:viewport-height}
      lens-keys #{:viewport-top :point :last-explicit-j}]
  (def edit-context
    "Perform some action in an \"edit context\".

    An \"edit context\" is the minimal information from a document and a lens,
    combined in such a way that a function can make edits to the file and move
    the cursor and viewport.
    
    This is intended to be used with packthread's \"in\" macro, like so:

      (+> editor
        (in e/edit-context
          (assoc :foo :bar)))"
    (beep/add-beep-to-focus
      (fn edit-context*
        ([editor]
         (merge
           (-> editor
               (get-in (current-document-path editor))
               (select-keys document-keys))
           (-> (current-lens editor)
               (select-keys lens-keys))
           (let [[_ [height _]] (::layout/shape (p/current-pane editor))]
             {:viewport-height (dec height)})))
        ([editor new-context]
         (-> editor
           (update-in (current-document-path editor) merge (select-keys new-context document-keys))
           (update-in (current-lens-path editor) merge (select-keys new-context lens-keys))))))))

;; -- Modes ------------------------------------------------------------------

(defn enter-normal-mode
  [editor]
  (assoc editor :mode :normal :message nil))

(defn mode-middleware
  [mode mode-responder]
  (fn [responder]
    (fn [editor event]
      (if (= mode (:mode editor))
        (mode-responder editor event)
        (responder editor event)))))

;; -- Terminal resizing ------------------------------------------------------

(defn wrap-handle-resize
  [responder]
  (fn [editor [event-type size :as event]]
    (if (= event-type :resize)
      (+> editor
        (assoc-in [::layout/shape 1] size)
        (in edit-context
          ec/adjust-viewport-to-contain-point))
      (responder editor event))))

;; -- Exceptions and failures ------------------------------------------------

(defn wrap-handle-exceptions
  [responder]
  (fn [editor event]
    (try
      (responder editor event)
      (catch Throwable e
        (merge editor (ex-data e))))))

;; ---------------------------------------------------------------------------
