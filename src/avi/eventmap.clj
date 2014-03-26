(ns avi.eventmap)

(defn- wrap-handler-with-beep-reset
  [handler]
  (fn [editor]
    (handler (assoc editor :beep? false))))

(defn- wrap-handler-with-repeat-loop
  [handler]
  (fn [editor]
    (let [repeat-count (or (:count editor) 1)]
      (nth (iterate handler editor) repeat-count))))

(defn- wrap-handler-with-count-reset
  [handler]
  (fn [editor]
    (assoc (handler editor) :count nil)))

(defn make-handler
  [& args]
  (let [tags (into #{} (take-while keyword? args))
        [handler] (drop-while keyword? args)]
    (cond-> handler
      true                     wrap-handler-with-beep-reset
      (not (:no-repeat tags))  wrap-handler-with-repeat-loop
      (not (:keep-count tags)) wrap-handler-with-count-reset)))

(defn- handler-fn
  [handler-args body]
  (let [editor-arg (first handler-args)
        repeat-arg (second handler-args)

        body (if-not repeat-arg
               `(do ~@body)
               `(let [~repeat-arg (:count ~editor-arg)]
                  ~@body))]
    `(fn [~editor-arg]
       ~body)))

(defn split-event-spec
  [key-sequence]
  (loop [remaining key-sequence
         result []]
    (cond
      (not (seq remaining))
      result

      (= \< (first remaining))
      (let [key-name (apply str (concat (take-while #(not= % \>) remaining) [\>]))
            remaining (->> remaining
                           (drop-while #(not= % \>))
                           rest)]
        (recur remaining (conj result key-name)))

      :else
      (recur (rest remaining) (conj result (str (first remaining)))))))

(defn- make-handler-name
  [event-spec]
  (let [name (symbol (str "on-" event-spec))]
    (with-meta name {:on-events event-spec})))

(defmacro on-events
  [& args]
  (let [tags (take-while keyword? args)
        [keystroke handler-args & handler-body] (drop-while keyword? args)
        handler-name (make-handler-name keystroke)

        tags (case (count handler-args)
               1 tags
               2 (conj tags :no-repeat))]
    `(def ~handler-name (make-handler ~@tags ~(handler-fn handler-args handler-body)))))

(defmacro on-unhandled-event
  [& args]
  (let [tags (take-while keyword? args)
        after-tags (drop-while keyword? args)]
    `(on-events ~@tags "" ~@after-tags)))

(defn eventmap
  [a-namespace]
  (reduce
    (fn [the-key-map a-fn]
      (if-let [keystroke (:on-events (meta a-fn))]
        (assoc the-key-map keystroke a-fn)
        the-key-map))
    {}
    (vals (ns-interns a-namespace))))

(defn invoke-event-handler
  [eventmap editor event]
  (let [event-handler-fn (or (get eventmap event)
                             (get eventmap "")
                             identity)]
    (event-handler-fn editor)))
