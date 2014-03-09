(ns avi.test-helpers
  (:import [java.io FileNotFoundException])
  (:require [avi.core :as core]
            [avi.render :as render]))

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

(defn- make-events-from-specification
  [spec]
  (loop [spec-left spec
         events []]
    (cond
      (not (seq spec-left))
      events

      (= \< (first spec-left))
      (let [event-name (apply str (concat (take-while #(not= % \>) spec-left) [\>]))
            spec-left (drop 1 (drop-while #(not= % \>) spec-left))
            event-type (if (.startsWith event-name "<Resize ")
                         :resize
                         :keystroke)
            event-data (if (= :resize event-type)
                         (read-string (apply str (drop 1 (drop-while #(not= % \space) event-name))))
                         event-name)
            event [event-type event-data]]
        (recur spec-left (conj events event)))

      (char? (first spec-left))
      (recur (rest spec-left)
             (conj events [:keystroke (str (first spec-left))])))))

(defn editor
  [& {file-contents :editing,
      event-spec :after
      :or {file-contents "One\nTwo\nThree\n."
           keystrokes ""}}]
  (let [events (make-events-from-specification event-spec)
        start-args (if (= :nothing file-contents)
                     []
                     ["test.txt"])
        slurp-redef (if (= :not-found file-contents)
                      (fn [_] (throw (FileNotFoundException. "not found")))
                      {"test.txt" file-contents})
        initial-editor (with-redefs [slurp slurp-redef]
                         (apply core/start [8 20] start-args))]
    (reduce
      core/process
      initial-editor
      events)))

(defn cursor
  [& args]
  (:cursor (render/render (apply editor args))))

(defn beeped
  [editor]
  (:beep? editor))

(defn did-not-beep
  [editor]
  (not (:beep? editor)))
