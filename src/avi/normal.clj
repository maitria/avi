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

(defn- move-to-end-of-line
  [editor]
  (let [b (e/current-buffer editor)
        [i j] (b/cursor b)
        line-length (count (b/line b i))
        j (max 0 (dec line-length))]
    (change-column editor (constantly j))))

(defn- update-count
  [editor digit]
  (let [old-count (or (:count editor) 0)
        new-count (+ (* 10 old-count) digit)]
    (assoc editor :count new-count)))

(defn- handle-0
  [editor]
  (if (:count editor)
    (update-count editor 0)
    (change-column editor (constantly 0))))

(defn- handle-G
  [editor]
  (let [last-line (b/line-count (e/current-buffer editor))
        target-line (or (:count editor) last-line)]
    (change-line editor (constantly (dec target-line)))))

(defn- current-line 
  [editor] 
  (let [buffer (e/current-buffer editor)
        [row column] (b/cursor buffer)]
    (b/line buffer row)))

(defn- index-of-first-non-blank
  [line]
  (let [leading-space-count (count (re-find #"^\s*" line))
        all-spaces? (and (> leading-space-count 0)
                         (= leading-space-count (count line)))]
    (if all-spaces?
      (dec leading-space-count)
      leading-space-count)))

(defn- move-to-first-non-blank-column
  [editor]
  (let [position (index-of-first-non-blank (current-line editor))]
    (change-column editor (constantly position))))

(defn- scroll
  [editor update-fn]
  (e/update-current-buffer editor #(b/scroll % update-fn)))

(defn- scroll-down-half-page
  [editor]
  (let [buffer (e/current-buffer editor)]
    (if (b/on-last-line? buffer)
      (beep editor)
      (e/update-current-buffer editor #(b/move-and-scroll-half-page % :down)))))

(defn- scroll-up-half-page
  [editor]
  (let [buffer (e/current-buffer editor)
        [i] (b/cursor buffer)]
    (if (zero? i)
      (beep editor)
      (e/update-current-buffer editor #(b/move-and-scroll-half-page % :up)))))

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
        handler (last args)]
    (cond-> handler
      true                      wrap-handler-with-beep-reset
      (not (:no-repeat? tags))  wrap-handler-with-repeat-loop
      (not (:keep-count? tags)) wrap-handler-with-count-reset)))

(def ^:private key-map
  {\return (make-handler #(assoc % :mode :finished))
   \0 (make-handler :keep-count? :no-repeat? handle-0)
   \1 (make-handler :keep-count? :no-repeat? #(update-count % 1))
   \2 (make-handler :keep-count? :no-repeat? #(update-count % 2))
   \3 (make-handler :keep-count? :no-repeat? #(update-count % 3))
   \4 (make-handler :keep-count? :no-repeat? #(update-count % 4))
   \5 (make-handler :keep-count? :no-repeat? #(update-count % 5))
   \6 (make-handler :keep-count? :no-repeat? #(update-count % 6))
   \7 (make-handler :keep-count? :no-repeat? #(update-count % 7))
   \8 (make-handler :keep-count? :no-repeat? #(update-count % 8))
   \9 (make-handler :keep-count? :no-repeat? #(update-count % 9))
   \^ (make-handler move-to-first-non-blank-column)
   \$ (make-handler move-to-end-of-line)
   \h (make-handler #(change-column % dec))
   \j (make-handler #(change-line % inc))
   \k (make-handler #(change-line % dec))
   \l (make-handler #(change-column % inc))
   \G (make-handler :no-repeat? handle-G)
   (ctrl \D) (make-handler scroll-down-half-page)
   (ctrl \E) (make-handler #(scroll % inc))
   (ctrl \U) (make-handler scroll-up-half-page)
   (ctrl \Y) (make-handler #(scroll % dec))})

(defn- key-handler
  [key]
  (or (get key-map key)
      {:handler beep}))

(defn process
  [editor key]
  ((key-handler key) editor))
