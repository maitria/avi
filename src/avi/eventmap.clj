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

(defn- events
  [event-spec]
  (->> (split-event-spec event-spec)
       (map #(vector :keystroke %))
       vec))

(defmacro on-events
  [& args]
  (let [[tags [event-spec handler-args & handler-body]] (split-with keyword? args)
        handler-symbol (symbol (str "on-" event-spec))
        handler-symbol (with-meta handler-symbol {:on-events (events event-spec)})
        tags (case (count handler-args)
               1 tags
               2 (conj tags :no-repeat))]
    `(def ~handler-symbol (make-handler ~@tags ~(handler-fn handler-args handler-body)))))

(defmacro on-unhandled-event
  [& args]
  (let [tags (take-while keyword? args)
        after-tags (drop-while keyword? args)]
    `(on-events ~@tags "<Default>" ~@after-tags)))

(defn eventmap
  [a-namespace]
  (reduce
    (fn [the-key-map a-fn]
      (if-let [[event & more-events] (:on-events (meta a-fn))]
        (assoc the-key-map event a-fn)
        the-key-map))
    {}
    (vals (ns-interns a-namespace))))

(defn invoke-event-handler
  [eventmap editor event]
  (let [event-handler-fn (or (get eventmap event)
                             (get eventmap [:keystroke "<Default>"])
                             identity)]
    (event-handler-fn editor)))
