(ns avi.editor
  "Functions (including basse responders, middleware, and utilties) for
   manipulating the editor map."
  (:require [packthread.core :refer :all]
            [packthread.lenses :as l]
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
      ([{:keys [focused-window] :as editor}]
       (get-in editor [:buffers (:buffer (current-window editor))]))
      ([{:keys [focused-window windows] :as editor} new-buffer]
       (assoc-in editor [:buffers (:buffer (current-window editor))] new-buffer)))))

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
