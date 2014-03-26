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
  [tags handler]
  (let [tags (into #{} tags)]
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

(defmacro on-unhandled-event
  [& args]
  (let [tags (take-while keyword? args)
        after-tags (drop-while keyword? args)]
    `(on-events ~@tags "<Default>" ~@after-tags)))

(defmacro eventmap
  [& mappings]
  (reduce
    (fn [eventmap args]
      (let [tag? #(and (keyword? %)
                       (not= :else %))
            [tags [event-spec handler-args & handler-body]] (split-with tag? args)
            tags (case (count handler-args)
                   1 tags
                   2 (conj tags :no-repeat))
            tags (vec tags)
            events (if (= :else event-spec)
                     :else
                     (first (events event-spec)))]
        (assoc eventmap events `(make-handler ~tags ~(handler-fn handler-args handler-body)))))
    {}
    mappings))

(defn invoke-event-handler
  [eventmap editor event]
  (let [event-handler-fn (or (get eventmap event)
                             (get eventmap :else)
                             identity)]
    (event-handler-fn editor)))
