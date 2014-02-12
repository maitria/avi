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

(defn process
  [editor key]
  (let [editor (assoc editor :beep? false)]
    (cond
      (= key :enter)
      (assoc editor :mode :finished)

      (= key \h)
      (change-column editor dec)

      (= key \j)
      (change-line editor inc)

      (= key \k)
      (change-line editor dec)

      (= key \l)
      (change-column editor inc)

      :else
      (assoc editor :beep? true))))

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
