(ns viv.core
  (:require [lanterna.screen :as lanterna]
            [clojure.string :as string])
  (:gen-class))

(defn start
  [[lines columns] filename]
  {:mode :normal
   :buffer {:name filename,
            :lines (string/split (slurp filename) #"\n")
            :cursor [0 0]}
   :lines lines
   :columns columns
   :beep? false})

(defn- valid-cursor-position?
  [editor [i j]]
  (and (>= i 0)
       (>= j 0)
       (< i (count (get-in editor [:buffer :lines])))
       (< j (count (get-in editor [:buffer :lines i])))))

(defn- move-cursor
  [editor [i-delta j-delta]]
  (let [[i j] (get-in editor [:buffer :cursor])
        new-position [(+ i i-delta) (+ j j-delta)]]
    (if (valid-cursor-position? editor new-position)
      (assoc-in editor [:buffer :cursor] new-position)
      (assoc editor :beep? true))))

(defn process
  [editor key]
  (let [editor (assoc editor :beep? false)]
    (cond
      (= key :enter)
      (assoc editor :mode :finished)

      (= key \h)
      (move-cursor editor [0 -1])

      (= key \j)
      (move-cursor editor [+1 0])

      (= key \k)
      (move-cursor editor [-1 0])

      (= key \l)
      (move-cursor editor [0 +1])

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
