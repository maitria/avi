(ns avi.editor
  "Functions (including basse responders, middleware, and utilties) for
   manipulating the editor map."
  (:import [java.io FileNotFoundException])
  (:require [clojure.set :as set]
            [packthread.core :refer :all]
            [packthread.lenses :as l]
            [avi.pervasive :refer :all]
            [avi.beep :as beep]
            [avi.edit-context :as ec]
            [avi.edit-context
              [lines :as lines]]
            [avi.panes :as p]
            [avi.world :as w]))

;; -- Initial state ----------------------------------------------------------

(defn- try-load
  [filename]
  (try
    (lines/content (w/read-file w/*world* filename))
    (catch FileNotFoundException e
      [""])))

(defn initial-editor
  [[lines columns] [filename]]
  {:mode :normal
   :documents [{:name filename,
                :lines (if filename
                         (try-load filename)
                         [""])
                :undo-log ()
                :redo-log ()
                :in-transaction? false}]
   :viewport {:size [lines columns]}
   :lenses [{:document 0
             :viewport-top 0
             :point [0 0]
             :last-explicit-j 0}]
   ::p/tree {:avi.panes/lens 0}
   ::p/path []
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

(let [document-keys #{:lines :undo-log :redo-log :in-transaction?}
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
           (let [[_ [height _]] (::p/shape (p/current-pane editor))]
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
        (assoc-in [:viewport :size] size)
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
