(ns avi.buffer
  (:import [java.io FileNotFoundException])
  (:require [packthread.core :refer :all]
            [clojure.string :as string]
            [avi.beep :as beep]
            [avi.buffer
              [lines :as lines]
              [locations :as l]]
            [avi.pervasive :refer :all]
            [avi.world :as w]
            [schema.core :as s]))

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
     :cursor [0 0],
     :last-explicit-j 0
     :undo-log ()
     :redo-log ()}))

;; Lenses

(let [lines-and-cursor-keys [:viewport-top :viewport-height :lines :cursor :last-explicit-j :beep? :message]]
  (defn lines-and-cursor
    ([buffer]
     (select-keys buffer lines-and-cursor-keys))
    ([buffer updated-lines]
     {:post [(or (:in-transaction? %)
                 (= (:lines buffer) (:lines %)))]}
     (merge buffer (select-keys updated-lines lines-and-cursor-keys)))))

;; --

(defn write
  [{filename :name,
    :as buffer}]
  (let [{:keys [lines]} (lines-and-cursor buffer)]
    (w/write-file w/*world* filename (string/join "\n" lines)))
  buffer)

(defn- adjust-viewport-to-contain-cursor
  [buffer]
  (+> buffer
    (let [height (:viewport-height buffer)
          viewport-top (:viewport-top buffer)
          viewport-bottom (dec (+ viewport-top height))
          [cursor-i] (:cursor buffer)]
      (cond
        (< cursor-i viewport-top)
        (assoc :viewport-top cursor-i)

        (> cursor-i viewport-bottom)
        (assoc :viewport-top (inc (- cursor-i height)))))))

(defn line
  [buffer i]
  (-> buffer lines-and-cursor :lines (get i)))

(defn j-within-line
  [buffer i]
  (let [j (:last-explicit-j buffer)
        line-length (count (line buffer i))
        j-not-after-end (min (dec line-length) j)
        j-within-line (max 0 j-not-after-end)]
    j-within-line))

(defn line-count
  [buffer]
  (-> buffer lines-and-cursor :lines count))

(defn viewport-middle
  [{top :viewport-top,
    height :viewport-height,
    :as buffer}]
  (let [middle-of-viewport (dec (+ top (quot height 2)))
        middle-of-file (quot (dec (line-count buffer)) 2)
        middle (min middle-of-viewport middle-of-file)]
    middle))

(defn move-cursor
  [{:keys [lines] :as buffer} [i j] & [explicit?]]
  (+> buffer
    (let [i (case i
              :current         (get-in buffer [:cursor 0])
              :viewport-middle (viewport-middle buffer)
              i)
          j (case j
              :end-of-line     (max 0 (dec (count (get lines i))))
              :first-non-blank (index-of-first-non-blank (get lines i))
              :last-explicit   (j-within-line buffer i)
              j)]
      (assoc :cursor [i j])
      (if explicit?
        (assoc :last-explicit-j j))
      adjust-viewport-to-contain-cursor)))

(defn- adjust-cursor-to-viewport
  [{:keys [viewport-top viewport-height]
    [i] :cursor
    :as buffer}]
  (+> buffer
    (let [viewport-bottom (dec (+ viewport-top viewport-height))]
      (cond
        (< i viewport-top)
        (move-cursor [viewport-top :last-explicit])

        (> i viewport-bottom)
        (move-cursor [viewport-bottom :last-explicit])))))

(defn resize
  [buffer height]
  (+> buffer
      (assoc :viewport-height height)
      (adjust-viewport-to-contain-cursor)))

(defn scroll
  [buffer scroll-fn]
  (+> buffer
      (update-in [:viewport-top] scroll-fn)
      (adjust-cursor-to-viewport)))

(defn on-last-line?
  [buffer]
  (let [[i] (:cursor buffer)
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

(defn- clamp-cursor-row
  [{top :viewport-top,
    height :viewport-height,
    :as buffer}
   new-top]
  (max 0 (min (dec (line-count buffer)) new-top)))

(defn- clamped-j
  [{[i] :cursor,
    :as buffer}
   j]
  (max 0 (min j (dec (count (line buffer i))))))

(defn- clamp-cursor-j
  [{[i j] :cursor,
    :as buffer}]
  (assoc buffer :cursor [i (clamped-j buffer j)]))

(defn cursor-can-move-to-column?
  [buffer j]
  (= j (clamped-j buffer j)))

(defn move-and-scroll-half-page
  [{top :viewport-top,
    height :viewport-height,
    [i] :cursor,
    :as buffer}
   which-way]
  (+> buffer
      (let [distance (quot height 2)
            direction (case which-way
                        :down +1
                        :up -1)
            scroll-adjust (* direction distance)]
        (move-cursor [(clamp-cursor-row buffer (+ i scroll-adjust)) :last-explicit])
        (scroll (constantly (clamp-viewport-top buffer (+ top scroll-adjust)))))))

(defn cursor-to-bottom-of-viewport
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
        (move-cursor [new-line :last-explicit]))))

(defn cursor-to-top-of-viewport
  [{top :viewport-top,
    :as buffer}
   count-from-top]
  (move-cursor buffer [(+ top count-from-top) :last-explicit]))

;; Changes, undo, redo

(defn start-transaction
  [{lines :lines,
    cursor :cursor,
    :as buffer}]
  (when (:in-transaction? buffer)
    (throw (Exception. "attempt to nest a transaction")))
  (+> buffer
    (update-in [:undo-log] conj {:lines lines, :cursor cursor})
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
    cursor :cursor,
    :as buffer}]
  (+> buffer
    (if-not (seq (from-log buffer))
      (beep/beep (str "Already at the " last-name " change"))
      (do
        (update-in [to-log] conj {:lines lines, :cursor cursor})
        (merge (first (from-log buffer)))
        (update-in [from-log] rest)
        adjust-viewport-to-contain-cursor))))

(def undo (partial undo-or-redo :undo-log :redo-log "oldest"))
(def redo (partial undo-or-redo :redo-log :undo-log "newest"))

;; -- changing buffer contents --

(s/defn change
  "All content changes happen through me!"
  [{:keys [cursor] :as buffer}
   a :- l/Location
   b :- l/Location
   replacement :- s/Str
   bias :- l/AdjustmentBias]
  (+> buffer
    (let [[_ j :as new-cursor] (l/adjust-for-replacement cursor a b replacement bias)]
      (update-in [:lines] lines/replace a b replacement)
      (if new-cursor
        (move-cursor new-cursor true)))))

(defn insert-text
  [{cursor :cursor, :as lines-and-text} text]
  (change lines-and-text cursor cursor text :right))

(defn delete-char-under-cursor
  [{[i j] :cursor,
    :as buffer}]
  {:pre [(:in-transaction? buffer)]}
  (+> buffer
    (change [i j] [i (inc j)] "" :left)
    clamp-cursor-j))

(defn delete-current-line
  [{[i] :cursor,
    lines :lines,
    :as buffer}]
  {:pre [(:in-transaction? buffer)]}
  (+> buffer
    (cond
      (= 1 (line-count buffer))
      (do
        (change [i 0] [i (count (get lines i))] "" :left)
        (move-cursor [0 0]))

      (= i (dec (line-count buffer)))
      (do
        (change [(dec i) (count (get lines (dec i)))] [i (count (get lines i))] "" :left)
        (move-cursor [(dec i) :first-non-blank]))

      :else
      (do
        (change [i 0] [(inc i) 0] "" :left)
        (move-cursor [i :first-non-blank])))))

(defn backspace
  [{cursor :cursor,
    lines :lines,
    :as buffer}]
  {:pre [(:in-transaction? buffer)]}
  (+> buffer
    (if-let [pre (l/retreat cursor (lines/line-length lines))]
      (change pre cursor "" :left))))
