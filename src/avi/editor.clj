(ns avi.editor
  "Functions (including basse responders, middleware, and utilties) for
   manipulating the editor map."
  (:require [packthread.core :refer :all]
            [packthread.lenses :as l]
            [schema.core :as s]
            [avi.pervasive :refer :all]
            [avi.beep :as beep]
            [avi.buffer :as b]))

;; -- Initial state ----------------------------------------------------------

(defn initial-editor
  [[lines columns] [filename]]
  {:mode :normal
   :documents [(b/open filename)]
   :viewport {:size [lines columns]}
   :windows [{:document 0
              :viewport-top 0
              :viewport-height (- lines 2)
              :point [0 0]
              :last-explicit-j 0}]
   :focused-window 0
   :beep? false})

;; -- Building middlewares ---------------------------------------------------

(defn keystroke-middleware
  [keystroke a-fn]
  (fn [handler]
    (fn [editor event]
      (if (= event [:keystroke keystroke])
        (a-fn editor)
        (handler editor event)))))

;; -- Tracking the current window & buffer -----------------------------------

(def current-window
  (beep/add-beep-to-focus
    (fn current-window*
      ([{:keys [focused-window] :as editor}]
       (get-in editor [:windows focused-window])))))

(def EditContext
  {:name s/Any
   :lines s/Any
   :viewport-top s/Any
   :viewport-height s/Any
   :point s/Any
   :last-explicit-j s/Any
   :undo-log s/Any
   :redo-log s/Any
   (s/optional-key :in-transaction?) s/Any})

(defn current-document-path
  [editor]
  [:documents (:document (current-window editor))])

(def ^:private edit-context-buffer-keys
  #{:name :lines :undo-log :redo-log :in-transaction?})
(def ^:private edit-context-window-keys
  #{:viewport-top :viewport-height :point :last-explicit-j})

(def edit-context
  "Perform some action in an \"edit context\".

  An \"edit context\" is the minimal information from a buffer and a window,
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
             (select-keys edit-context-buffer-keys))
           (-> (current-window editor)
             (select-keys edit-context-window-keys)))))
      ([{:keys [focused-window] :as editor} new-context]
       (-> editor
         (update-in (current-document-path editor) merge (select-keys new-context edit-context-buffer-keys))
         (update-in [:windows focused-window] merge (select-keys new-context edit-context-window-keys)))))))

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
              (b/resize (- (first size) 2))))
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
