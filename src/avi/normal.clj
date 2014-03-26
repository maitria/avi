(ns avi.normal
  (:require [avi.buffer :as b]
            [avi.editor :as e]
            [avi.eventmap :as em]))

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

(em/mapkey "<Enter>"
  [editor]
  (assoc editor :mode :finished))

(em/mapkey :keep-count "0"
  [editor repeat-count]
  (if repeat-count
    (update-count editor 0)
    (change-column editor (constantly 0))))

(em/mapkey :keep-count "1"
  [editor repeat-count]
  (update-count editor 1))
(em/mapkey :keep-count "2"
  [editor repeat-count]
  (update-count editor 2))
(em/mapkey :keep-count "3"
  [editor repeat-count]
  (update-count editor 3))
(em/mapkey :keep-count "4"
  [editor repeat-count]
  (update-count editor 4))
(em/mapkey :keep-count "5"
  [editor repeat-count]
  (update-count editor 5))
(em/mapkey :keep-count "6"
  [editor repeat-count]
  (update-count editor 6))
(em/mapkey :keep-count "7"
  [editor repeat-count]
  (update-count editor 7))
(em/mapkey :keep-count "8"
  [editor repeat-count]
  (update-count editor 8))
(em/mapkey :keep-count "9"
  [editor repeat-count]
  (update-count editor 9))

(em/mapkey "^"
  [editor]
  (let [position (index-of-first-non-blank (current-line editor))]
    (change-column editor (constantly position))))

(em/mapkey "$"
  [editor]
  (let [b (e/current-buffer editor)
        [i j] (b/cursor b)
        line-length (count (b/line b i))
        j (max 0 (dec line-length))]
    (change-column editor (constantly j))))

(em/mapkey "g"
  [editor]
  (change-line editor (constantly 0)))

(em/mapkey "h"
  [editor]
  (change-column editor dec))

(em/mapkey "j"
  [editor]
  (change-line editor inc))

(em/mapkey "k"
  [editor]
  (change-line editor dec))

(em/mapkey "l"
  [editor]
  (change-column editor inc))

(em/mapkey "G"
  [editor repeat-count]
  (let [last-line (b/line-count (e/current-buffer editor))
        target-line (or repeat-count last-line)]
    (change-line editor (constantly (dec target-line)))))

(em/mapkey "H"
  [editor repeat-count]
  (let [count (dec (or repeat-count 1))]
    (e/update-current-buffer editor #(b/cursor-to-top-of-viewport % count))))

(em/mapkey "L"
  [editor repeat-count]
  (let [count (dec (or repeat-count 1))]
    (e/update-current-buffer editor #(b/cursor-to-bottom-of-viewport % count))))

(em/mapkey "M"
  [editor]
  (e/update-current-buffer editor b/cursor-to-middle-of-viewport))

(em/mapkey "<C-D>"
  [editor]
  (let [buffer (e/current-buffer editor)]
    (if (b/on-last-line? buffer)
      (beep editor)
      (e/update-current-buffer editor #(b/move-and-scroll-half-page % :down)))))

(em/mapkey "<C-E>"
  [editor]
  (scroll editor inc))

(em/mapkey "<C-U>"
  [editor]
  (let [buffer (e/current-buffer editor)
        [i] (b/cursor buffer)]
    (if (zero? i)
      (beep editor)
      (e/update-current-buffer editor #(b/move-and-scroll-half-page % :up)))))

(em/mapkey "<C-Y>"
  [editor]
  (scroll editor dec))

(def key-map (em/ns->keymap 'avi.normal))

(defn- key-handler
  [key]
  (or (get key-map key) beep))

(defn process
  [editor key]
  ((key-handler key) editor))
