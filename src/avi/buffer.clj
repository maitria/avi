(ns avi.buffer
  (:import [java.io FileNotFoundException])
  (:require [packthread.core :refer :all]
            [clojure.string :as string]
            [avi.beep :as beep]
            [avi.buffer
              [lines :as lines]
              [locations :as l]
              [move]]
            [avi.pervasive :refer :all]
            [avi.world :as w]
            [schema.core :as s]
            [potemkin :refer [import-vars]]))

(import-vars [avi.buffer.move
                adjust-viewport-to-contain-point
                move-point])

(defn- try-load
  [filename]
  (try
    (lines/content (w/read-file w/*world* filename))
    (catch FileNotFoundException e
      [""])))

(defn open
  [filename height]
  (let [lines (if filename
                (try-load filename)
                [""])]
    {:name filename,
     :viewport-top 0
     :viewport-height height
     :lines lines,
     :point [0 0],
     :last-explicit-j 0
     :undo-log ()
     :redo-log ()}))

;; Lenses

(let [lines-and-point-keys [:viewport-top :viewport-height :lines :point :last-explicit-j :beep? :message]]
  (defn lines-and-point
    ([buffer]
     (select-keys buffer lines-and-point-keys))
    ([buffer updated-lines]
     {:post [(or (:in-transaction? %)
                 (= (:lines buffer) (:lines %)))]}
     (merge buffer (select-keys updated-lines lines-and-point-keys)))))

;; --

(defn write
  [{filename :name,
    :as buffer}]
  (let [{:keys [lines]} (lines-and-point buffer)]
    (w/write-file w/*world* filename (string/join "\n" lines)))
  buffer)

(defn line
  [buffer i]
  (-> buffer lines-and-point :lines (get i)))

(defn line-count
  [buffer]
  (-> buffer lines-and-point :lines count))

(defn- adjust-point-to-viewport
  [{:keys [viewport-top viewport-height]
    [i] :point
    :as buffer}]
  (+> buffer
    (let [viewport-bottom (dec (+ viewport-top viewport-height))]
      (cond
        (< i viewport-top)
        (move-point [:to [viewport-top :last-explicit]])

        (> i viewport-bottom)
        (move-point [:to [viewport-bottom :last-explicit]])))))

(defn resize
  [buffer height]
  (+> buffer
      (assoc :viewport-height height)
      (adjust-viewport-to-contain-point)))

(defn scroll
  [buffer scroll-fn]
  (+> buffer
      (update-in [:viewport-top] scroll-fn)
      (adjust-point-to-viewport)))

(defn on-last-line?
  [buffer]
  (let [[i] (:point buffer)
        line-count (line-count buffer)]
    (= i (dec line-count))))

(defn- clamp-viewport-top
  [{top :viewport-top,
    height :viewport-height,
    :as buffer}
   new-top]
  (let [line-count (line-count buffer)
        max-top (max 0 (- line-count height))]
    (min max-top (max 0 new-top))))

(defn- clamp-point-row
  [{top :viewport-top,
    height :viewport-height,
    :as buffer}
   new-top]
  (max 0 (min (dec (line-count buffer)) new-top)))

(defn- clamped-j
  [{[i] :point,
    :as buffer}
   j]
  (max 0 (min j (dec (count (line buffer i))))))

(defn- clamp-point-j
  [{[i j] :point,
    :as buffer}]
  (assoc buffer :point [i (clamped-j buffer j)]))

(defn point-can-move-to-column?
  [buffer j]
  (= j (clamped-j buffer j)))

(defn move-and-scroll-half-page
  [{top :viewport-top,
    height :viewport-height,
    [i] :point,
    :as buffer}
   which-way]
  (+> buffer
      (let [distance (quot height 2)
            direction (case which-way
                        :down +1
                        :up -1)
            scroll-adjust (* direction distance)]
        (move-point [:to [(clamp-point-row buffer (+ i scroll-adjust)) :last-explicit]])
        (scroll (constantly (clamp-viewport-top buffer (+ top scroll-adjust)))))))

(defn point-to-bottom-of-viewport
  [{top :viewport-top,
    height :viewport-height,
    :as buffer}
   count-from-bottom]
  (+> buffer
      (let [bottom-of-viewport (dec (+ top height))
            bottom-of-file (dec (line-count buffer))
            count-from-bottom-of-viewport (- bottom-of-viewport count-from-bottom)
            count-from-bottom-of-file (- bottom-of-file count-from-bottom)
            new-line (max top (min count-from-bottom-of-viewport count-from-bottom-of-file))]
        (move-point [:to [new-line :last-explicit]]))))

(defn point-to-top-of-viewport
  [{top :viewport-top,
    :as buffer}
   count-from-top]
  (move-point buffer [:to [(+ top count-from-top) :last-explicit]]))

;; Changes, undo, redo

(defn start-transaction
  [{lines :lines,
    point :point,
    :as buffer}]
  (when (:in-transaction? buffer)
    (throw (Exception. "attempt to nest a transaction")))
  (+> buffer
    (update-in [:undo-log] conj {:lines lines, :point point})
    (assoc :in-transaction? true)))

(defn commit
  [buffer]
  (+> buffer
      (assoc :in-transaction? false
             :redo-log ())))

(defn- undo-or-redo
  [from-log
   to-log
   last-name
   {lines :lines,
    point :point,
    :as buffer}]
  (+> buffer
    (if-not (seq (from-log buffer))
      (beep/beep (str "Already at the " last-name " change"))
      (do
        (update-in [to-log] conj {:lines lines, :point point})
        (merge (first (from-log buffer)))
        (update-in [from-log] rest)
        adjust-viewport-to-contain-point))))

(def undo (partial undo-or-redo :undo-log :redo-log "oldest"))
(def redo (partial undo-or-redo :redo-log :undo-log "newest"))

;; -- changing buffer contents --

(s/defn change
  "All content changes happen through me!"
  [{:keys [point] :as buffer}
   a :- l/Location
   b :- l/Location
   replacement :- s/Str
   bias :- l/AdjustmentBias]
  (+> buffer
    (let [[_ j :as new-point] (l/adjust-for-replacement point a b replacement bias)]
      (update-in [:lines] lines/replace a b replacement)
      (if new-point
        (move-point [:to new-point])))))

(defn insert-text
  [{point :point, :as lines-and-text} text]
  (change lines-and-text point point text :right))

(defn delete-char-under-point
  [{[i j] :point,
    :as buffer}]
  {:pre [(:in-transaction? buffer)]}
  (+> buffer
    (change [i j] [i (inc j)] "" :left)
    clamp-point-j))

(defn delete-current-line
  [{[i] :point,
    lines :lines,
    :as buffer}]
  {:pre [(:in-transaction? buffer)]}
  (+> buffer
    (cond
      (= 1 (line-count buffer))
      (do
        (change [i 0] [i (count (get lines i))] "" :left)
        (move-point [:to [0 0]]))

      (= i (dec (line-count buffer)))
      (do
        (change [(dec i) (count (get lines (dec i)))] [i (count (get lines i))] "" :left)
        (move-point [:to [(dec i) :first-non-blank]]))

      :else
      (do
        (change [i 0] [(inc i) 0] "" :left)
        (move-point [:to [i :first-non-blank]])))))

(defn backspace
  [{point :point,
    lines :lines,
    :as buffer}]
  {:pre [(:in-transaction? buffer)]}
  (+> buffer
    (if-let [pre (l/retreat point (lines/line-length lines))]
      (change pre point "" :left))))
