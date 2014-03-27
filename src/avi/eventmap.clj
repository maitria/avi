(ns avi.eventmap)

(defn wrap-handler-with-beep-reset
  [handler]
  (fn [editor]
    (handler (assoc editor :beep? false))))

(defn wrap-handler-with-repeat-loop
  [handler]
  (fn [editor]
    (let [repeat-count (or (:count editor) 1)]
      (nth (iterate handler editor) repeat-count))))

(defn wrap-handler-with-count-reset
  [handler]
  (fn [editor]
    (assoc (handler editor) :count nil)))

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

(defn- tag?
  [tag]
  (and (keyword? tag)
       (not= :else tag)))

(defn- parse-eventmap-entry
  [entry-form]
  (let [[tags [event-spec args & body]] (split-with tag? entry-form)]
    {:event-spec event-spec,
     :args args,
     :body body
     :tags (into #{} tags)}))

(defn- entry-handler-fn
  [{:keys [tags args body]}]
  (let [editor-arg (first args)
        repeat-arg (second args)

        body (if-not repeat-arg
               `(do ~@body)
               `(let [~repeat-arg (:count ~editor-arg)]
                  ~@body))

        repeat-loop? (and (not (:no-repeat tags)) (not repeat-arg))
        reset-count? (not (:keep-count tags))

        wrappers (cond-> `[wrap-handler-with-beep-reset]
                   repeat-loop? (conj `wrap-handler-with-repeat-loop)
                   reset-count? (conj `wrap-handler-with-count-reset))]
    `(-> (fn [~editor-arg]
           ~body)
         ~@wrappers)))

(defn make-handler
  [tags handler]
  handler)

(defmacro eventmap
  [& mappings]
  (reduce
    (fn [eventmap args]
      (let [entry (parse-eventmap-entry args)
            tags (case (count (:args entry))
                   1 (:tags entry)
                   2 (conj (:tags entry) :no-repeat))
            eventmap-key (if (= :else (:event-spec entry))
                           :else
                           (first (events (:event-spec entry))))]
        (assoc eventmap eventmap-key `(make-handler ~tags ~(entry-handler-fn entry)))))
    {}
    mappings))

(defn invoke-event-handler
  [eventmap editor event]
  (let [event-handler-fn (or (get eventmap event)
                             (get eventmap :else)
                             identity)]
    (event-handler-fn editor)))
