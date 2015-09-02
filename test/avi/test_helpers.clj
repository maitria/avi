(ns avi.test-helpers
  (:import [java.io FileNotFoundException])
  (:require [avi.color :as color]
            [avi.core]
            [avi.editor :as e]
            [avi.eventmap :as em]
            [avi.world :refer :all]
            [clojure.string :as string]
            [midje.checking.core :as checking]))

(def ten-lines
  (str "One\nTwo\nThree\nFour\nFive\nSix\n"
       "Seven\nEight\nNine\nTen"))

(defn- event
  [event-name]
  (let [event-type (if (.startsWith event-name "<Resize ")
                     :resize
                     :keystroke)
        event-data (if (= :resize event-type)
                     (read-string (apply str (drop 1 (drop-while #(not= % \space) event-name))))
                     event-name)]
    [event-type event-data]))

(defn- make-events
  [string-of-commands]
  (->> string-of-commands
       (em/split-string-of-commands)
       (map event)))

(defn editor
  [& {width :width,
      file-contents :editing,
      string-of-commands :after,
      :or {width 40,
           file-contents "One\nTwo\nThree\n.",
           keystrokes ""}}]
  (let [events (make-events string-of-commands)
        start-args (if (= :nothing file-contents)
                     []
                     ["test.txt"])
        file-written (atom nil)
        test-world (reify
                     World
                     (read-file [_ filename]
                       (cond
                         (= :not-found file-contents)
                         (throw (FileNotFoundException. "not found"))

                         (= "test.txt" filename)
                         file-contents))
                     (write-file [_ filename content]
                       (swap! file-written (constantly [filename content]))
                       nil))
        initial-editor (binding [*world* test-world]
                         (avi.main/initial-editor [8 width] start-args))
        final-editor (binding [*world* test-world]
                       (reduce
                         avi.main/responder
                         initial-editor
                         events))]
    {:editor final-editor
     :file-written @file-written}))

(defn wrote-file
  [expected-filename expected-contents]
  (fn [{[filename contents] :file-written}]
    (and
      (checking/extended-= filename expected-filename)
      (checking/extended-= contents expected-contents))))

(defn- line-keeper
  [line]
  (cond
    (not line)
    (fn [i v]
      v)

    (= line :message)
    (fn [i v]
      (if (= i 7)
        v))

    :else
    (fn [i v]
      (if (= i line)
        v))))

(defn- unwrap-single-value
  [coll]
  (if (= 1 (count coll))
    (first coll)
    coll))

(defn line
  [line expected]
  (fn [{{{:keys [width chars attrs]} :rendition} :editor}]
    (let [height (quot (count chars) width)
          lines (->> (range height)
                     (map #(String. chars (* % width) width))
                     (map string/trimr))
          line-annotations (->> (range height)
                                (map (fn [i]
                                       (get attrs (* i width))))
                                (map color/description))]
      (checking/extended-=
        (->> (map vector lines line-annotations)
             (keep-indexed (line-keeper line))
             flatten
             unwrap-single-value)
        expected))))
(def message-line (partial line :message))

(defn terminal
  [expected]
  (line nil expected))

(defn attributes
  [[i j] expected]
  (fn [{{{:keys [width attrs]} :rendition} :editor}]
    (-> (get attrs (+ j (* i width)))
      color/description
      unwrap-single-value
      (checking/extended-= expected))))

(defn cursor
  [expected-pos]
  (fn [{:keys [editor]}]
    (checking/extended-= (:cursor (:rendition editor)) expected-pos)))

(defn beeped
  [{:keys [editor]}]
  (:beep? editor))

(defn did-not-beep
  [{:keys [editor]}]
  (not (:beep? editor)))

(defn mode
  [expected-mode]
  (fn [{{:keys [mode]} :editor}]
    (checking/extended-= mode expected-mode)))

(def finished? (comp :finished? :editor))
(def unfinished? (complement finished?))

(defn viewport-size
  [expected-size]
  (fn [{{{:keys [size]} :viewport} :editor}]
    (checking/extended-= size expected-size)))
