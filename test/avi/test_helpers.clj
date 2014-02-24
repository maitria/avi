(ns avi.test-helpers
  (:require [avi.core :as core]
            [avi.render :as render]))

(defn renders
  [expected & {[i j] :at,
               expected-color :in,
               expected-background :on,
               :or {expected-color :white,
                    expected-background :black}}]
  (fn [editor]
    (let [{:keys [chars attrs width]} (render/render editor)
          expected-attrs (render/make-attrs expected-color expected-background)
          expected-chars (->> (range (count expected))
                              (map (partial + j))
                              (map #(vector %1 i %2) expected))]
      (every?
        (fn [[c i j]]
          (let [index (+ j (* i width))]
            (and
              (= (get chars index) c)
              (= (get attrs index) expected-attrs))))
        expected-chars))))

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
                         (core/start [10 15] "test/test.txt"))]
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
