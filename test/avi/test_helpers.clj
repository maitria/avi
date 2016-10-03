(ns avi.test-helpers
  (:import [java.io FileNotFoundException])
  (:require [midje.sweet :refer :all]
            [avi.color :as color]
            [avi.core]
            [avi.editor :as e]
            [avi.events :as ev]
            [avi.main]
            [avi.world :refer :all]
            [avi.test-helpers properties]
            [clojure.spec.test :as spec.test]
            [clojure.string :as string]
            [midje.checking.core :as checking]
            [potemkin :refer [import-vars]]))

(spec.test/instrument)

(import-vars [avi.test-helpers.properties
                property])

(def ten-lines
  (str "One\nTwo\nThree\nFour\nFive\nSix\n"
       "Seven\nEight\nNine\nTen"))

(def ten-lines-indented
  (str "  One\n"
       "  Two\n"
       "  Three\n"
       "  Four\n"
       "  Five\n"
       "  Six\n"
       "  Seven\n"
       "  Eight\n"
       "  Nine\n"
       "  Ten"))


(defn- compare-result
  [result expected custom-msg]
  (or (checking/extended-= result expected)
      (checking/as-data-laden-falsehood {:notes [(str custom-msg (pr-str result))]})))

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
       (ev/split-string-of-commands)
       (map event)))

(def ^:private responder
  (avi.main/responder false))

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
                         responder
                         initial-editor
                         events))]
    {:editor final-editor
     :file-written @file-written}))

(defn wrote-file
  [expected-filename expected-contents]
  (fn [{[filename contents] :file-written}]
    (compare-result [filename contents] [expected-filename expected-contents] "Failed [filename contents]:")))

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

(defn line-cmp
  [line expected ncmpbuf]
  (fn [{{{:keys [width chars attrs]} :rendition} :editor}]
    (let [height (if ncmpbuf (quot (count chars) width) (- (quot (count chars) width) 2))
          lines (->> (range height)
                     (map #(String. chars (* % width) width))
                     (map string/trimr))
          line-annotations (->> (range height)
                                (map (fn [i]
                                       (get attrs (* i width))))
                                (map color/description))
          result (->> (map vector lines line-annotations)
             (keep-indexed (line-keeper line))
             flatten
             unwrap-single-value)]
    (compare-result result expected "Failed set:"))))
(defn line
  [line expected]
  (line-cmp line expected true))
(def message-line (partial line :message))

(defn terminal
  [expected]
  (line nil expected))

(defn terminal-buffer
  ;; Added to compare only lines in buffer without status and command line
  [expected]
  (line-cmp nil expected false))

(defn contents
  [expected]
  (fn [editor]
    (compare-result (string/join "\n" (:lines (e/edit-context (:editor editor)))) expected "Failed contents:")))

(defn attributes
  [[i j] expected]
  (fn [{{{:keys [width attrs]} :rendition} :editor}]
    (-> (get attrs (+ j (* i width)))
      color/description
      unwrap-single-value
      (compare-result expected "Failed attributes:"))))

(defn point
  [expected-pos]
  (fn [{:keys [editor]}]
    (compare-result (:point (:rendition editor)) expected-pos "Failed point:")))

(defn beeped
  [{:keys [editor]}]
  (:beep? editor))

(defn did-not-beep
  [{:keys [editor]}]
  (not (:beep? editor)))

(defn mode
  [expected-mode]
  (fn [{{:keys [mode]} :editor}]
    (compare-result mode expected-mode "Failed mode:")))

(def finished? (comp :finished? :editor))
(def unfinished? (complement finished?))

(defn viewport-size
  [expected-size]
  (fn [{{{:keys [width chars]} :rendition} :editor}]
    (let [height (int (/ (count chars) width))
          size [height width]]
    (compare-result size expected-size "Failed viewport-size:"))))
