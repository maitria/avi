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

(defn move-cursor
  [editor [i-delta j-delta]]
  (let [[i j] (get-in editor [:buffer :cursor])
        new-i (+ i i-delta)
        new-j (+ j j-delta)]
    (if (and (>= new-i 0)
             (< new-i (count (get-in editor [:buffer :lines]))))
      (assoc-in editor [:buffer :cursor] [new-i new-j])
      (assoc editor :beep? true))))

(defn process-key
  [editor key]
  (let [editor (assoc editor :beep? false)]
    (cond
      (= key :enter)
      (assoc editor :mode :finished)

      (= key \j)
      (move-cursor editor [+1 0])

      (= key \k)
      (move-cursor editor [-1 0])

      :else
      editor)))

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
        (recur (process-key editor (lanterna/get-key-blocking screen)))))
    (lanterna/stop screen)))
