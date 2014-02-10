(ns viv.core-test
  (:use midje.sweet)
  (:require [viv.core :as core]))

(defn displays
  [expected & {[i j] :at,
               expected-color :in,
               expected-background :on,
               :or {expected-color :white,
                    expected-background :black}}]
  (fn [editor]
    (let [screen-lines (:lines (core/render editor))
          [actual-color actual-background line] (get screen-lines i)
          actual (.substring line j (+ j (count expected)))]
      (and
        (= expected actual)
        (= expected-color actual-color)
        (= expected-background actual-background)))))

(defn editor-after-typing
  [keys]
  (reduce
    #(core/process-key %1 %2)
    (core/start [10 80] "test/test.txt")
    keys))

(defn cursor-after-typing
  [keys]
  (:cursor (core/render (editor-after-typing keys))))

(facts "regarding displaying of a loaded file"
  (let [editor (core/start [10 80] "test/test.txt")]
    (fact "Each line is displayed on a different line."
      editor => (displays "One" :at [0 0])
      editor => (displays "Two" :at [1 0])
      editor => (displays "Three" :at [2 0]))

    (fact "Tildes are displayed on blank lines."
      editor => (displays "~" :at [3 0] :in :blue)
      editor => (displays "~" :at [7 0] :in :blue))

    (fact "The filename appears in the status bar."
      editor => (displays "test/test.txt" :at [8 0] :in :black :on :white))))

(facts "regarding cursor movement"
  (fact "The cursor starts on line 1, column 0."
    (cursor-after-typing "") => [0 0])
  (fact "j moves the cursor down one line."
    (cursor-after-typing "j") => [1 0]
    (cursor-after-typing "jj") => [2 0])
  (fact "k moves the cursor up one line."
    (cursor-after-typing "jk") => [0 0])
    (cursor-after-typing "jjjkk") => [1 0])

(facts "regarding quitting"
  (fact "It doesn't start in the 'finished' state."
    (:mode (editor-after-typing "")) =not=> :finished
    (:mode (editor-after-typing ":")) =not=> :finished
    (:mode (editor-after-typing ":q")) =not=> :finished)
  (fact "It exits after :q<CR>."
    (:mode (-> (editor-after-typing ":q")
               (core/process-key :enter))) => :finished))
