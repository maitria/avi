(ns viv.core
  (:require [lanterna.screen :as lanterna]
            [clojure.string :as string])
  (:gen-class))

(defn start
  [[lines columns] filename]
  {:mode :normal
   :buffer {:name filename,
            :lines (string/split (slurp filename) #"\n"),
            :cursor [0 0],
            :last-explicit-j 0}
   :lines lines
   :columns columns
   :count nil
   :beep? false})

(defn- valid-column?
  [editor [i j]]
  (and (>= j 0)
       (< j (count (get-in editor [:buffer :lines i])))))

(defn- change-column
  [editor j-fn]
  (let [[i j] (get-in editor [:buffer :cursor])
        j (j-fn j)
        new-position [i j]]
    (if (valid-column? editor new-position)
      (-> editor
          (assoc-in [:buffer :cursor] new-position)
          (assoc-in [:buffer :last-explicit-j] j))
      (assoc editor :beep? true))))

(defn- valid-line?
  [editor i]
  (or (< i 0)
      (>= i (count (get-in editor [:buffer :lines])))))

(defn- j-within-line
  [editor [i j]]
  (let [j (get-in editor [:buffer :last-explicit-j])
        line-length (count (get-in editor [:buffer :lines i]))
        j-not-after-end (min (dec line-length) j)
        j-within-line (max 0 j-not-after-end)]
    j-within-line))

(defn- change-line
  [editor i-fn]
  (let [[i j] (get-in editor [:buffer :cursor])
        i (i-fn i)
        j (j-within-line editor [i j])]
    (if (valid-line? editor i)
      (assoc editor :beep? true)
      (assoc-in editor [:buffer :cursor] [i j]))))

(defn- move-to-end-of-line
  [editor]
  (let [[i j] (get-in editor [:buffer :cursor])
        line-length (count (get-in editor [:buffer :lines i]))
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
  (let [last-line (count (get-in editor [:buffer :lines]))
        target-line (or (:count editor) last-line)]
    (change-line editor (constantly (dec target-line)))))

(def ^:private key-map
  {:enter {:handler #(assoc % :mode :finished)}
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
   \$ {:handler move-to-end-of-line}
   \h {:handler #(change-column % dec)}
   \j {:handler #(change-line % inc)}
   \k {:handler #(change-line % dec)}
   \l {:handler #(change-column % inc)}
   \G {:handler handle-G, :no-repeat? true}})

(defn- beep
  [editor]
  (assoc editor :beep? true))

(defn- key-handler
  [editor key]
  (or (get key-map key)
      {:handler beep}))

(defn process
  [editor key]
  (let [repeat-count (or (:count editor) 1)
        {:keys [handler keep-count? no-repeat?]} (key-handler editor key)
        handler (if no-repeat?
                  handler
                  #(nth (iterate handler %) repeat-count))
        handler (if keep-count?
                  handler
                  #(assoc (handler %) :count nil))
        editor (assoc editor :beep? false)]
    (handler editor)))

(defn render
  [editor]
  (let [buffer-lines (map #(vector :white :black %) (get-in editor [:buffer :lines]))
        tilde-lines (repeat [:blue :black "~"])
        status-line [:black :white (get-in editor [:buffer :name])]]
    {:lines (vec
              (concat
                (take (- (:lines editor) 2) (concat buffer-lines tilde-lines))
                [status-line])),
     :cursor (get-in editor [:buffer :cursor])}))

(defn- update-screen
  [editor screen]
  (let [rendition (render editor)
        screen-lines (:lines rendition)
        [cursor-i cursor-j] (:cursor rendition)]
    (doseq [i (range (count screen-lines))]
      (let [[color background text] (get screen-lines i)]
        (lanterna/put-string screen 0 i text {:bg background, :fg color})))
    (lanterna/move-cursor screen cursor-j cursor-i)
    (lanterna/redraw screen)))

(defn -main
  [filename]
  (let [screen (lanterna/get-screen :unix)]
    (lanterna/start screen)
    (loop [editor (let [[columns lines] (lanterna/get-size screen)]
                    (start [lines columns] filename))]
      (update-screen editor screen)
      (if-not (= (:mode editor) :finished)
        (recur (process editor (lanterna/get-key-blocking screen)))))
    (lanterna/stop screen)))
