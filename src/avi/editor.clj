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
   :buffers [(b/open filename (- lines 2))]
   :viewport {:size [lines columns]}
   :windows [{:buffer 0}]
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
         (-> editor
           (get-in [:buffers (:buffer (current-window editor))])
           (select-keys [:name :lines :viewport-top :viewport-height :point :last-explicit-j :undo-log :redo-log :in-transaction?]))))
      ([editor new-context]
       (update-in editor [:buffers (:buffer (current-window editor))] merge new-context)))))

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
