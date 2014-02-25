(ns avi.test-helpers
  (:require [avi.core :as core]
            [avi.render :as render]))

(defn- text-matches-rendering?
  [rendering i text]
  (let [{:keys [chars width]} rendering 
        actual-text (String. chars (* i width) width)]
    (= text actual-text)))

(defn- color-matches-rendering?
  [rendering i color]
  (let [{:keys [attrs width]} rendering
        offset (* i width)
        line-attrs (subvec (into [] attrs) offset (+ offset width))
        expected-attrs (case color
                         :white (render/make-attrs :white :black)
                         :blue (render/make-attrs :blue :black)
                         :inverse (render/make-attrs :black :white))]
    (->> line-attrs
         (filter (partial not= color))
         seq)))

(defn- partition-lines
  [lines]
  (loop [[text maybe-attributes & lines] lines
         result []]
    (cond
      (not text)
      result

      (keyword? maybe-attributes)
      (recur lines
             (conj result [text maybe-attributes]))

      :else
      (recur (cons maybe-attributes lines)
             (conj result [text :white])))))

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
