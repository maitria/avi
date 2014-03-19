(ns avi.normal
  (:require [avi.buffer :as b]
            [avi.editor :as e]
            [avi.keymap :refer :all]))

(defn- beep
  [editor]
  (assoc editor :beep? true))

(defn- cursor-can-move-to-column?
  [editor [i j]]
  (let [line-length (count (b/line (e/current-buffer editor) i))
        inside-line? (and (>= j 0)
                          (< j line-length))
        column-zero? (zero? j)]
    (or inside-line?
        column-zero?)))

(defn- change-column
  [editor j-fn]
  (let [[i j] (b/cursor (e/current-buffer editor))
        j (j-fn j)
        new-position [i j]]
    (if (cursor-can-move-to-column? editor new-position)
      (e/update-current-buffer editor #(b/move-cursor % new-position j))
      (beep editor))))

(defn- valid-line?
  [editor i]
  (and (>= i 0)
       (< i (b/line-count (e/current-buffer editor)))))

(defn- change-line
  [editor i-fn]
  (let [[i] (b/cursor (e/current-buffer editor))
        i (i-fn i)]
    (if-not (valid-line? editor i)
      (beep editor)
      (e/update-current-buffer editor #(b/move-to-line % i)))))

(defn- update-count
  [editor digit]
  (let [old-count (or (:count editor) 0)
        new-count (+ (* 10 old-count) digit)]
    (assoc editor :count new-count)))

(defn- current-line 
  [editor] 
  (let [buffer (e/current-buffer editor)
        [row] (b/cursor buffer)]
    (b/line buffer row)))

(defn- index-of-first-non-blank
  [line]
  (let [leading-space-count (count (re-find #"^\s*" line))
        all-spaces? (and (> leading-space-count 0)
                         (= leading-space-count (count line)))]
    (if all-spaces?
      (dec leading-space-count)
      leading-space-count)))

(defn- scroll
  [editor update-fn]
  (e/update-current-buffer editor #(b/scroll % update-fn)))

(mapkey "<Enter>"
  [editor]
  (assoc editor :mode :finished))

(mapkey :keep-count "0"
  [editor repeat-count]
  (if repeat-count
    (update-count editor 0)
    (change-column editor (constantly 0))))

(mapkey :keep-count "1"
  [editor repeat-count]
  (update-count editor 1))
(mapkey :keep-count "2"
  [editor repeat-count]
  (update-count editor 2))
(mapkey :keep-count "3"
  [editor repeat-count]
  (update-count editor 3))
(mapkey :keep-count "4"
  [editor repeat-count]
  (update-count editor 4))
(mapkey :keep-count "5"
  [editor repeat-count]
  (update-count editor 5))
(mapkey :keep-count "6"
  [editor repeat-count]
  (update-count editor 6))
(mapkey :keep-count "7"
  [editor repeat-count]
  (update-count editor 7))
(mapkey :keep-count "8"
  [editor repeat-count]
  (update-count editor 8))
(mapkey :keep-count "9"
  [editor repeat-count]
  (update-count editor 9))

(mapkey "^"
  [editor]
  (let [position (index-of-first-non-blank (current-line editor))]
    (change-column editor (constantly position))))

(mapkey "$"
  [editor]
  (let [b (e/current-buffer editor)
        [i j] (b/cursor b)
        line-length (count (b/line b i))
        j (max 0 (dec line-length))]
    (change-column editor (constantly j))))

(mapkey "h"
  [editor]
  (change-column editor dec))

(mapkey "j"
  [editor]
  (change-line editor inc))

(mapkey "k"
  [editor]
  (change-line editor dec))

(mapkey "l"
  [editor]
  (change-column editor inc))

(mapkey "G"
  [editor repeat-count]
  (let [last-line (b/line-count (e/current-buffer editor))
        target-line (or repeat-count last-line)]
    (change-line editor (constantly (dec target-line)))))

(mapkey "H"
  [editor repeat-count]
  (let [count (dec (or repeat-count 1))]
    (e/update-current-buffer editor #(b/cursor-to-top-of-viewport % count))))

(mapkey "L"
  [editor repeat-count]
  (let [count (dec (or repeat-count 1))]
    (e/update-current-buffer editor #(b/cursor-to-bottom-of-viewport % count))))

(mapkey "M"
  [editor]
  (e/update-current-buffer editor b/cursor-to-middle-of-viewport))

(mapkey "<C-D>"
  [editor]
  (let [buffer (e/current-buffer editor)]
    (if (b/on-last-line? buffer)
      (beep editor)
      (e/update-current-buffer editor #(b/move-and-scroll-half-page % :down)))))

(mapkey "<C-E>"
  [editor]
  (scroll editor inc))

(mapkey "<C-U>"
  [editor]
  (let [buffer (e/current-buffer editor)
        [i] (b/cursor buffer)]
    (if (zero? i)
      (beep editor)
      (e/update-current-buffer editor #(b/move-and-scroll-half-page % :up)))))

(mapkey "<C-Y>"
  [editor]
  (scroll editor dec))

(def key-map (ns->keymap 'avi.normal))

(defn- key-handler
  [key]
  (or (get key-map key) beep))

(defn process
  [editor key]
  ((key-handler key) editor))
