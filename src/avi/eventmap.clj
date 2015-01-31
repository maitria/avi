(ns avi.eventmap
  (:require [avi.editor :as e]))

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

(defn- parse-eventmap-entry
  [entry-form]
  (let [[event-spec args & body] entry-form]
    {:event-spec event-spec,
     :args args,
     :body body}))

(defn- entry-handler-fn
  [{:keys [args body]}]
  (let [arg-named (fn [the-name]
                    (some->> args
                      (filter #(= (name %) the-name))
                      first))
        editor-arg (arg-named "editor")
        repeat-arg (arg-named "repeat-count")

        body (if-not repeat-arg
               `(do ~@body)
               `(let [~repeat-arg (:count ~editor-arg)]
                  ~@body))

        repeat-loop? (not repeat-arg)

        wrappers (cond-> `(wrap-handler-with-count-reset)
                   repeat-loop? (conj `wrap-handler-with-repeat-loop))]
    `(-> (fn [~editor-arg event#]
           ~body)
         ~@wrappers)))

(defn invoke-event-handler
  [eventmap]
  (fn [responder]
    (fn [editor event]
      (let [event-path (conj (or (:pending-events editor) []) event)
            event-handler-fn (get-in eventmap event-path)]
        (cond
          (not event-handler-fn)
          (responder editor event)

          (map? event-handler-fn)
          (assoc editor :pending-events event-path)

          :else
          (-> editor
              (event-handler-fn event)
              (assoc :pending-events [])))))))

(defmacro eventmap
  [& mappings]
  (let [em (reduce
             (fn [eventmap args]
               (let [entry (parse-eventmap-entry args)
                     event-path (events (:event-spec entry))]
                 (assoc-in eventmap event-path (entry-handler-fn entry))))
             {}
             mappings)]
    `(invoke-event-handler ~em)))

(defn beep-responder
  [editor event]
  (e/beep editor))
