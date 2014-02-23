(ns avi.normal
  (:require [avi.buffer :as buffer]
            [avi.editor :as editor]))

(defn- beep
  [editor]
  (assoc editor :beep? true))

(defn- valid-column?
  [editor [i j]]
  (and (>= j 0)
       (< j (count (buffer/line (editor/current-buffer editor) i)))))

(defn- change-column
  [editor j-fn]
  (let [[i j] (buffer/cursor (editor/current-buffer editor))
        j (j-fn j)
        new-position [i j]]
    (if (valid-column? editor new-position)
      (editor/update-current-buffer editor #(buffer/with-cursor % new-position j))
      (beep editor))))

(defn- valid-line?
  [editor i]
  (or (< i 0)
      (>= i (buffer/lines (editor/current-buffer editor)))))

(defn- j-within-line
  [editor [i j]]
  (let [b (editor/current-buffer editor)
        j (buffer/last-explicit-j b)
        line-length (count (buffer/line b i))
        j-not-after-end (min (dec line-length) j)
        j-within-line (max 0 j-not-after-end)]
    j-within-line))

(defn- change-line
  [editor i-fn]
  (let [[i j] (buffer/cursor (editor/current-buffer editor))
        i (i-fn i)
        j (j-within-line editor [i j])]
    (if (valid-line? editor i)
      (beep editor)
      (editor/update-current-buffer editor #(buffer/with-cursor % [i j])))))

(defn- move-to-end-of-line
  [editor]
  (let [b (editor/current-buffer editor)
        [i j] (buffer/cursor b)
        line-length (count (buffer/line b i))
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

(defn handle-G
  [editor]
  (let [last-line (buffer/lines (editor/current-buffer editor))
        target-line (or (:count editor) last-line)]
    (change-line editor (constantly (dec target-line)))))

(def ^:private key-map
  {\return {:handler #(assoc % :mode :finished)}
   \0 {:handler handle-0, :keep-count? true, :no-repeat? true}
   \1 {:handler #(update-count % 1), :keep-count? true, :no-repeat? true}
   \2 {:handler #(update-count % 2), :keep-count? true, :no-repeat? true}
   \3 {:handler #(update-count % 3), :keep-count? true, :no-repeat? true}
   \4 {:handler #(update-count % 4), :keep-count? true, :no-repeat? true}
   \5 {:handler #(update-count % 5), :keep-count? true, :no-repeat? true}
   \6 {:handler #(update-count % 6), :keep-count? true, :no-repeat? true}
   \7 {:handler #(update-count % 7), :keep-count? true, :no-repeat? true}
   \8 {:handler #(update-count % 8), :keep-count? true, :no-repeat? true}
   \9 {:handler #(update-count % 9), :keep-count? true, :no-repeat? true}
   \^ {:handler #(change-column % (constantly 0))}
   \$ {:handler move-to-end-of-line}
   \h {:handler #(change-column % dec)}
   \j {:handler #(change-line % inc)}
   \k {:handler #(change-line % dec)}
   \l {:handler #(change-column % inc)}
   \G {:handler handle-G, :no-repeat? true}})

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

(defn- key-handler
  [editor key]
  (let [{:keys [handler keep-count? no-repeat?]} (or (get key-map key)
                                                     {:handler beep})]
    (cond-> handler
      true              wrap-handler-with-beep-reset
      (not no-repeat?)  wrap-handler-with-repeat-loop
      (not keep-count?) wrap-handler-with-count-reset)))

(defn process
  [editor key]
  ((key-handler editor key) editor))
