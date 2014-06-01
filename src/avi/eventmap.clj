(ns avi.eventmap)

(defn wrap-handler-with-beep-reset
  [handler]
  (fn [editor event]
    (-> editor
        (assoc :beep? false)
        (handler event))))

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
  [string-of-commands]
  (->> (split-string-of-commands string-of-commands)
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
  (let [arg-named (fn [the-name]
                    (some->> args
                      (filter #(= (name %) the-name))
                      first))
        editor-arg (arg-named "editor")
        repeat-arg (arg-named "repeat-count")
        event-arg (or (arg-named "event") `event#)

        body (if-not repeat-arg
               `(do ~@body)
               `(let [~repeat-arg (:count ~editor-arg)]
                  ~@body))

        repeat-loop? (and (not (:no-repeat tags)) (not repeat-arg))
        reset-count? (not (:keep-count tags))

        wrappers (cond-> `[wrap-handler-with-beep-reset]
                   repeat-loop? (conj `wrap-handler-with-repeat-loop)
                   reset-count? (conj `wrap-handler-with-count-reset))]
    `(-> (fn [~editor-arg ~event-arg]
           ~body)
         ~@wrappers)))

(defmacro eventmap
  [& mappings]
  (reduce
    (fn [eventmap args]
      (let [entry (parse-eventmap-entry args)
            event-path (if (= :else (:event-spec entry))
                         [:else]
                         (events (:event-spec entry)))]
        (assoc-in eventmap event-path (entry-handler-fn entry))))
    {}
    mappings))

(defn- null-handler
  [editor event]
  editor)

(defn invoke-event-handler
  [eventmap editor event]
  (let [event-path (conj (or (:pending-events editor) []) event)
        event-handler-fn (or (get-in eventmap event-path)
                             (:else eventmap)
                             null-handler)]
    (if (map? event-handler-fn)
      (assoc editor :pending-events event-path)
      (-> editor
          (event-handler-fn event)
          (assoc :pending-events [])))))
