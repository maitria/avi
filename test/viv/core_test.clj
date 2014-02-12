(ns viv.core-test
  (:use midje.sweet)
  (:require [viv.core :as core]))

(defn renders
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

(defn editor
  [& {file-contents :when-editing,
      keystrokes :after-typing,
      :or {file-contents "One\nTwo\nThree\n."
           keystrokes ""}}]
  (reduce
    core/process
    (with-redefs [slurp (constantly file-contents)]
      (core/start [10 80] "test/test.txt"))
    keystrokes))

(defn cursor
  [& args]
  (:cursor (core/render (apply editor args))))

(defn beeped?
  [editor]
  (:beep? editor))

(facts "regarding displaying of a loaded file"
  (fact "Each line is displayed on a different line."
    (editor) => (renders "One" :at [0 0])
    (editor) => (renders "Two" :at [1 0])
    (editor) => (renders "Three" :at [2 0]))
  (fact "Tildes are displayed on blank lines."
    (editor) => (renders "~" :at [4 0] :in :blue)
    (editor) => (renders "~" :at [7 0] :in :blue))
  (fact "The filename appears in the status bar."
    (editor) => (renders "test/test.txt" :at [8 0] :in :black :on :white)))

(facts "regarding cursor movement"
  (fact "The cursor starts on line 1, column 0."
    (cursor) => [0 0])
  (fact "`j` moves the cursor down one line."
    (cursor :after-typing "j") => [1 0]
    (cursor :after-typing "jj") => [2 0])
  (fact "`j` can move to a zero-length line."
    (cursor :when-editing "One\n\nTwo" :after-typing "j") => [1 0])
  (fact "`j` won't move the cursor below the last line."
    (cursor :after-typing "jjjj") => [3 0]
    (editor :after-typing "jjjj") => beeped?)
  (fact "`j` won't place the cursor after the end of the line."
    (cursor :when-editing "Hello\nOne" :after-typing "llllj") => [1 2])
  (fact "`k` moves the cursor up one line."
    (cursor :after-typing "jk") => [0 0]
    (cursor :after-typing "jjjkk") => [1 0])
  (fact "`k` won't move above the first line."
    (cursor :after-typing "k") => [0 0]
    (editor :after-typing "k") => beeped?
    (editor :after-typing "kj") =not=> beeped?)
  (fact "`k` can move to a zero-length line."
    (cursor :when-editing "\nOne" :after-typing "jk") => [0 0])
  (fact "`l` moves to the right one character."
    (cursor :after-typing "l") => [0 1]
    (cursor :after-typing "ll") => [0 2])
  (fact "`l` won't move beyond the end of the line."
    (cursor :after-typing "lll") => [0 2]
    (editor :after-typing "lll") => beeped?)
  (fact "`h` moves to the left one character."
    (cursor :after-typing "llh") => [0 1])
  (fact "`h` won't move before the beginning of the line."
    (cursor :after-typing "h") => [0 0]
    (editor :after-typing "h") => beeped?))

(facts "regarding quitting"
  (fact "It doesn't start in the 'finished' state."
    (:mode (editor)) =not=> :finished
    (:mode (editor :after-typing ":")) =not=> :finished
    (:mode (editor :after-typing ":q")) =not=> :finished)
  (fact "It exits after `:q<CR>`."
    (:mode (-> (editor :after-typing ":q")
               (core/process :enter))) => :finished))
