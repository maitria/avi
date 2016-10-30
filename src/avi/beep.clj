(ns avi.beep
  (:require [packthread.core :refer :all]))

(defn error
  [editor-like message]
  (+> editor-like
    (assoc :message [:white :red message])))

(defn beep
  [editor-like & [message]]
  (+> editor-like
    (assoc :avi.editor/beep? true)
    (if message
      (error message))))

(defn beep-responder
  [editor event]
  (beep editor))

(defn unhandled-event-responder
  [editor event]
  (beep editor (str "Unhandled event " (pr-str event))))

(defn wrap-reset-beep
  [handler]
  (fn [editor event]
    (-> editor
        (assoc :avi.editor/beep? false)
        (handler event))))

(defn add-beep-to-focus
  "Modifies a lens to pass :beep? and :message into and out of the focus so
  that our error-signalling functions can be used within the focus."
  [lens]
  (fn
    ([original]
     (merge
       (lens original)
       (select-keys original [:avi.editor/beep? :message])))
    ([original new-value]
     (-> original
       (lens (dissoc new-value :avi.editor/beep? :message))
       (merge (select-keys new-value [:avi.editor/beep? :message]))))))
