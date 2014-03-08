(ns avi.normal
  (:require [avi.buffer :as b]
            [avi.editor :as e]))

(defn ctrl
  [c]
  (char (- (int c) 0x40)))

(defn- beep
  [editor]
  (assoc editor :beep? true))

(defn- cursor-can-move-to-column?
  [editor [i j]]
  (let [line-length (count (b/line (e/current-buffer editor) i))
        inside-line? (and (>= j 0)
                          (< j line-length))
        column-zero? (zero? j)]
    (or inside-line?
        column-zero?)))

(defn- change-column
  [editor j-fn]
  (let [[i j] (b/cursor (e/current-buffer editor))
        j (j-fn j)
        new-position [i j]]
    (if (cursor-can-move-to-column? editor new-position)
      (e/update-current-buffer editor #(b/move-cursor % new-position j))
      (beep editor))))

(defn- valid-line?
  [editor i]
  (and (>= i 0)
       (< i (b/line-count (e/current-buffer editor)))))

(defn- change-line
  [editor i-fn]
  (let [[i] (b/cursor (e/current-buffer editor))
        i (i-fn i)]
    (if-not (valid-line? editor i)
      (beep editor)
      (e/update-current-buffer editor #(b/move-to-line % i)))))

(defn- update-count
  [editor digit]
  (let [old-count (or (:count editor) 0)
        new-count (+ (* 10 old-count) digit)]
    (assoc editor :count new-count)))

(defn- current-line 
  [editor] 
  (let [buffer (e/current-buffer editor)
        [row] (b/cursor buffer)]
    (b/line buffer row)))

(defn- index-of-first-non-blank
  [line]
  (let [leading-space-count (count (re-find #"^\s*" line))
        all-spaces? (and (> leading-space-count 0)
                         (= leading-space-count (count line)))]
    (if all-spaces?
      (dec leading-space-count)
      leading-space-count)))

(defn- scroll
  [editor update-fn]
  (e/update-current-buffer editor #(b/scroll % update-fn)))

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

(defn- make-handler
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
        handler-name (with-meta (gensym) {:handles keystroke})]
    `(def ~handler-name (make-handler ~@tags (fn ~handler-args ~@handler-body)))))

(defhandler \return
  [editor]
  (assoc editor :mode :finished))

(defhandler :keep-count? :no-repeat? \0
  [editor]
  (if (:count editor)
    (update-count editor 0)
    (change-column editor (constantly 0))))

(defhandler :keep-count? :no-repeat? \1
  [editor]
  (update-count editor 1))
(defhandler :keep-count? :no-repeat? \2
  [editor]
  (update-count editor 2))
(defhandler :keep-count? :no-repeat? \3
  [editor]
  (update-count editor 3))
(defhandler :keep-count? :no-repeat? \4
  [editor]
  (update-count editor 4))
(defhandler :keep-count? :no-repeat? \5
  [editor]
  (update-count editor 5))
(defhandler :keep-count? :no-repeat? \6
  [editor]
  (update-count editor 6))
(defhandler :keep-count? :no-repeat? \7
  [editor]
  (update-count editor 7))
(defhandler :keep-count? :no-repeat? \8
  [editor]
  (update-count editor 8))
(defhandler :keep-count? :no-repeat? \9
  [editor]
  (update-count editor 9))

(defhandler \^
  [editor]
  (let [position (index-of-first-non-blank (current-line editor))]
    (change-column editor (constantly position))))

(defhandler \$
  [editor]
  (let [b (e/current-buffer editor)
        [i j] (b/cursor b)
        line-length (count (b/line b i))
        j (max 0 (dec line-length))]
    (change-column editor (constantly j))))

(defhandler \h
  [editor]
  (change-column editor dec))

(defhandler \j
  [editor]
  (change-line editor inc))

(defhandler \k
  [editor]
  (change-line editor dec))

(defhandler \l
  [editor]
  (change-column editor inc))

(defhandler :no-repeat? \G
  [editor]
  (let [last-line (b/line-count (e/current-buffer editor))
        target-line (or (:count editor) last-line)]
    (change-line editor (constantly (dec target-line)))))

(defhandler (ctrl \D)
  [editor]
  (let [buffer (e/current-buffer editor)]
    (if (b/on-last-line? buffer)
      (beep editor)
      (e/update-current-buffer editor #(b/move-and-scroll-half-page % :down)))))

(defhandler (ctrl \E)
  [editor]
  (scroll editor inc))

(defhandler (ctrl \U)
  [editor]
  (let [buffer (e/current-buffer editor)
        [i] (b/cursor buffer)]
    (if (zero? i)
      (beep editor)
      (e/update-current-buffer editor #(b/move-and-scroll-half-page % :up)))))

(defhandler (ctrl \Y)
  [editor]
  (scroll editor dec))

(defn default-key-map
  []
  (reduce
    (fn [the-key-map a-fn]
      (if-let [keystroke (:handles (meta a-fn))]
        (assoc the-key-map keystroke a-fn)
        the-key-map))
    {}
    (vals (ns-interns 'avi.normal))))

(def key-map (default-key-map))

(defn- key-handler
  [key]
  (or (get key-map key)
      {:handler beep}))

(defn process
  [editor key]
  ((key-handler key) editor))
