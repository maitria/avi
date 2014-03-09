(ns avi.keymap)

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

(defn- make-handler-name
  [keystroke]
  (let [name (symbol (str "handle-" keystroke))]
    (with-meta name {:handles keystroke})))

(defn make-handler
  [& args]
  (let [tags (into #{} (take-while keyword? args))
        [handler] (drop-while keyword? args)]
    (cond-> handler
      true                      wrap-handler-with-beep-reset
      (not (:no-repeat? tags))  wrap-handler-with-repeat-loop
      (not (:keep-count? tags)) wrap-handler-with-count-reset)))

(defmacro defhandler
  [& args]
  (let [tags (take-while keyword? args)
        [keystroke handler-args & handler-body] (drop-while keyword? args)
        handler-name (make-handler-name keystroke)]
    `(def ~handler-name (make-handler ~@tags (fn ~handler-args ~@handler-body)))))

(defn ns->keymap
  [a-namespace]
  (reduce
    (fn [the-key-map a-fn]
      (if-let [keystroke (:handles (meta a-fn))]
        (assoc the-key-map keystroke a-fn)
        the-key-map))
    {}
    (vals (ns-interns a-namespace))))
