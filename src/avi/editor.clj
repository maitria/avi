(ns avi.editor
  "Functions (including basse responders, middleware, and utilties) for
   manipulating the editor map."
  (:require [clojure.set :as set]
            [packthread.core :refer :all]
            [packthread.lenses :as l]
            [schema.core :as s]
            [avi.pervasive :refer :all]
            [avi.beep :as beep]
            [avi.edit-context :as ec]))

;; -- Initial state ----------------------------------------------------------

(defn initial-editor
  [[lines columns] [filename]]
  {:mode :normal
   :documents [(ec/open filename)]
   :viewport {:size [lines columns]}
   :lenses [{:document 0
             :viewport-top 0
             :viewport-height (- lines 2)
             :point [0 0]
             :last-explicit-j 0}]
   :focused-lens 0
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

(def current-lens
  (beep/add-beep-to-focus
    (fn current-lens*
      ([{:keys [focused-lens] :as editor}]
       (get-in editor [:lenses focused-lens])))))

(def EditContext
  {:name s/Any
   :lines s/Any
   :viewport-top s/Any
   :viewport-height s/Any
   :point s/Any
   :last-explicit-j s/Any
   :undo-log s/Any
   :redo-log s/Any
   :in-transaction? s/Any})

(defn current-document-path
  [editor]
  [:documents (:document (current-lens editor))])

(let [document-keys #{:name :lines :undo-log :redo-log :in-transaction?}
      lens-keys (set/difference
                  (into #{} (keys EditContext))
                  document-keys)]
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
         (s/validate
           EditContext
           (merge
             (-> editor
               (get-in (current-document-path editor))
               (select-keys document-keys))
             (-> (current-lens editor)
               (select-keys lens-keys)))))
        ([{:keys [focused-lens] :as editor} new-context]
         (-> editor
           (update-in (current-document-path editor) merge (select-keys new-context document-keys))
           (update-in [:lenses focused-lens] merge (select-keys new-context lens-keys))))))))

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
              (ec/resize (- (first size) 2))))
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
