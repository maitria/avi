(ns avi.test-helpers
  (:import [java.io FileNotFoundException])
  (:require [avi.core]
            [avi.editor :as e]
            [avi.eventmap :as em]
            [avi.render :as render]
            [avi.world :refer :all]))

(def ten-lines
  (str "One\nTwo\nThree\nFour\nFive\nSix\n"
       "Seven\nEight\nNine\nTen"))

(defn- text-matches-rendering?
  [rendering i text]
  (let [{:keys [chars width]} rendering 
        actual-text (String. chars (* i width) width)]
    (= text actual-text)))

(defn- color-matches-rendering?
  [rendering i expected-attrs]
  (let [{:keys [attrs width]} rendering
        offset (* i width)
        line-attrs (subvec (into [] attrs) offset (+ offset width))]
    (->> line-attrs
         (filter (partial not= expected-attrs))
         empty?)))

(defn- partition-looks-like-lines
  [lines]
  (loop [[text maybe-attributes & lines] lines
         result []]
    (cond
      (not text)
      result

      (vector? maybe-attributes)
      (let [[specified-foreground on specified-background] maybe-attributes
            foreground (or specified-foreground :white)
            background (or specified-background :black)
            attrs (render/make-attrs foreground background)]
        (recur lines
               (conj result [text attrs])))

      :else
      (recur (cons maybe-attributes lines)
             (conj result [text (render/make-attrs :white :black)])))))

(defn looks-like
  [& args]
  (fn [editor]
    (let [rendering (render/render editor)]
      (->> args
           partition-looks-like-lines
           (map-indexed 
             (fn [i [text color]]
               (and (text-matches-rendering? rendering i text)
                    (color-matches-rendering? rendering i color))))
           (every? identity)))))

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

(defn- simulate
  [& {file-contents :editing,
      string-of-commands :after
      :or {file-contents "One\nTwo\nThree\n."
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
                         (e/initial-editor [8 20] start-args))
        final-editor (binding [*world* test-world]
                       (reduce
                         e/respond
                         initial-editor
                         events))]
    {:editor final-editor
     :file-written @file-written}))

(defn file-written
  [& args]
  (:file-written (apply simulate args)))

(defn editor
  [& args]
  (:editor (apply simulate args)))

(defn cursor
  [& args]
  (:cursor (render/render (apply editor args))))

(defn beeped
  [editor]
  (:beep? editor))

(defn did-not-beep
  [editor]
  (not (:beep? editor)))
