(ns avi.test-helpers
  (:require [avi.core :as core]
            [avi.render :as render]))

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

(defn- partition-lines
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
           partition-lines
           (map-indexed 
             (fn [i [text color]]
               (and (text-matches-rendering? rendering i text)
                    (color-matches-rendering? rendering i color))))
           (every? identity)))))

(defn editor
  [& {file-contents :when-editing,
      keystrokes :after-typing,
      event :after-receiving
      :or {file-contents "One\nTwo\nThree\n."
           keystrokes ""}}]
  (let [key-events (map #(vector :keystroke %) keystrokes)
        events (concat key-events (if event
                                    [event]))
        initial-editor (with-redefs [slurp (constantly file-contents)]
                         (core/start [8 15] "test/test.txt"))]
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
