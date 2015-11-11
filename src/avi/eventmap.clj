(ns avi.eventmap
  (:require [avi.editor :as e]
            [packthread.core :refer :all]))

(defn wrap-handler-with-repeat-loop
  [handler]
  (fn [editor event]
    (let [repeat-count (or (:count editor) 1)]
      (nth (iterate #(handler % event) editor) repeat-count))))

(defn wrap-handler-with-count-reset
  [handler]
  (fn [editor event]
    (-> editor
        (handler event)
        (assoc :count nil))))

(defn split-string-of-commands
  [key-sequence]
  (lazy-seq
    (if-let [[_ key rest] (re-matches #"^(<[^<]+>|[^<])(.*)$" (or key-sequence ""))]
      (cons key (split-string-of-commands rest)))))

(defn- events
  [string-of-commands]
  (->> (split-string-of-commands string-of-commands)
       (map #(vector :keystroke %))
       vec))

(defmacro eventfn
  [args & body]
  (let [arg-named (fn [the-name]
                    (some->> args
                             (filter #(= (name %) the-name))
                             first))
        editor-arg (arg-named "editor")
        repeat-arg (arg-named "repeat-count")

        body (if-not repeat-arg
               `(do ~@body)
               `(let [~repeat-arg (:count ~editor-arg)]
                  ~@body))]
    `(fn [~editor-arg event#]
       ~body)))

(defn get-with-wildcards
  "Like get, except that a key of [:keystroke \"<.>\"] matches any key event."
  [m k]
  (or (get m k)
      (and (= (first k) :keystroke)
           (get m [:keystroke "<.>"]))))

(defn get-in-with-wildcards
  "Like get-in, except that a key of [:keystroke \"<.>\"] matches any key
  event."
  [eventmap [path-first & path-rest :as path]]
  (if (empty? path)
    eventmap
    (if-let [submap (get-with-wildcards eventmap path-first)]
      (recur submap path-rest))))

(defn invoke-event-handler
  [eventmap]
  (fn [responder]
    (fn [editor event]
      (let [event-path (conj (or (:pending-events editor) []) event)
            event-handler-fn (get-in-with-wildcards eventmap event-path)]
        (+> editor
          (cond
            (not event-handler-fn)
            (responder event)

            (map? event-handler-fn)
            (assoc :pending-events event-path)

            :else
            (do
              (assoc :pending-events event-path)
              (event-handler-fn event)
              (assoc :pending-events []))))))))

(defn decorate-event-handler
  [f]
  (+> f
    (if-not (:no-repeat (meta f))
      wrap-handler-with-repeat-loop)
    wrap-handler-with-count-reset))

(defn eventmap
  [mappings]
  (let [em (reduce
             (fn [eventmap [event-spec f]]
               (let [event-path (events event-spec)
                     f (decorate-event-handler f)]
                 (assoc-in eventmap event-path f)))
             {}
             mappings)]
    (invoke-event-handler em)))
