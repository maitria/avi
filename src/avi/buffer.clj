(ns avi.buffer
  (:import [java.io FileNotFoundException])
  (:require [packthread.core :refer :all]
            [clojure.string :as string]
            [avi.beep :as beep]
            [avi.buffer
              [change]
              [lines :as lines]
              [locations :as l]
              [operate]
              [transactions]]
            [avi.pervasive :refer :all]
            [avi.world :as w]
            [potemkin :refer [import-vars]]))

(import-vars [avi.buffer.change
                adjust-viewport-to-contain-point
                change]
             [avi.buffer.operate
                clamp-point-j
                operate]
             [avi.buffer.transactions
                start-transaction
                commit])

(defn- try-load
  [filename]
  (try
    (lines/content (w/read-file w/*world* filename))
    (catch FileNotFoundException e
      [""])))

(defn open
  [filename]
  (let [lines (if filename
                (try-load filename)
                [""])]
    {:name filename,
     :lines lines,
     :undo-log ()
     :redo-log ()}))

;; --

(defn write
  [{filename :name,
    :keys [lines]
    :as edit-context}]
  (w/write-file w/*world* filename (string/join "\n" lines))
  edit-context)

(defn line
  [edit-context i]
  (-> edit-context :lines (get i)))

(defn line-count
  [edit-context]
  (-> edit-context :lines count))

(defn- adjust-point-to-viewport
  [{:keys [viewport-top viewport-height]
    [i] :point
    :as edit-context}]
  (+> edit-context
    (let [viewport-bottom (dec (+ viewport-top viewport-height))]
      (cond
        (< i viewport-top)
        (operate {:operator :move-point
                  :motion [:goto [viewport-top]]})

        (> i viewport-bottom)
        (operate {:operator :move-point
                  :motion [:goto [viewport-bottom]]})))))

(defn resize
  [edit-context height]
  (+> edit-context
      (assoc :viewport-height height)
      (adjust-viewport-to-contain-point)))

(defn scroll
  [edit-context scroll-fn]
  (+> edit-context
      (update-in [:viewport-top] scroll-fn)
      (adjust-point-to-viewport)))

(defn on-last-line?
  [edit-context]
  (let [[i] (:point edit-context)
        line-count (line-count edit-context)]
    (= i (dec line-count))))

(defn- clamp-viewport-top
  [{top :viewport-top,
    height :viewport-height,
    :as edit-context}
   new-top]
  (let [line-count (line-count edit-context)
        max-top (max 0 (- line-count height))]
    (min max-top (max 0 new-top))))

(defn move-and-scroll-half-page
  [{top :viewport-top,
    height :viewport-height,
    [i] :point,
    :as edit-context}
   which-way]
  (+> edit-context
      (let [distance (quot height 2)
            direction (case which-way
                        :down +1
                        :up -1)
            scroll-adjust (* direction distance)]
        (operate {:operator :move-point
                  :motion [:goto [(+ i scroll-adjust)]]})
        (scroll (constantly (clamp-viewport-top edit-context (+ top scroll-adjust)))))))

;; -- undo & redo --

(defn- undo-or-redo
  [from-log
   to-log
   last-name
   {lines :lines,
    point :point,
    :as edit-context}]
  (+> edit-context
    (if-not (seq (from-log edit-context))
      (beep/beep (str "Already at the " last-name " change"))
      (do
        (update-in [to-log] conj {:lines lines, :point point})
        (merge (first (from-log edit-context)))
        (update-in [from-log] rest)
        adjust-viewport-to-contain-point))))

(def undo (partial undo-or-redo :undo-log :redo-log "oldest"))
(def redo (partial undo-or-redo :redo-log :undo-log "newest"))

;; -- changing edit-context contents --

(defn insert-text
  [{point :point, :as lines-and-text} text]
  (change lines-and-text point point text :right))

(defn delete-current-line
  [{[i] :point,
    lines :lines,
    :as edit-context}]
  {:pre [(:in-transaction? edit-context)]}
  (+> edit-context
    (cond
      (= 1 (line-count edit-context))
      (do
        (change [i 0] [i (count (get lines i))] "" :left)
        (operate {:operator :move-point
                  :motion [:goto [0 0]]}))

      (= i (dec (line-count edit-context)))
      (do
        (change [(dec i) (count (get lines (dec i)))] [i (count (get lines i))] "" :left)
        (operate {:operator :move-point
                  :motion [:goto [(dec i) :first-non-blank]]}))

      :else
      (do
        (change [i 0] [(inc i) 0] "" :left)
        (operate {:operator :move-point
                  :motion [:goto [i :first-non-blank]]})))))

(defn backspace
  [{point :point,
    lines :lines,
    :as edit-context}]
  {:pre [(:in-transaction? edit-context)]}
  (+> edit-context
    (if-let [pre (l/retreat point (lines/line-length lines))]
      (change pre point "" :left))))
