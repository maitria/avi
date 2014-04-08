(ns avi.normal-mode
  (:require [avi.buffer :as b]
            [avi.editor :as e]
            [avi.eventmap :as em]
            [avi.command-line-mode :as command-line-mode]))

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
      (e/beep editor))))

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

(def eventmap
  (em/eventmap
    ("<Enter>"
      [editor]
      (assoc editor :mode :finished))

    (:keep-count "0"
      [editor repeat-count]
      (if repeat-count
        (update-count editor 0)
        (change-column editor (constantly 0))))
    (:keep-count "1"
      [editor repeat-count]
      (update-count editor 1))
    (:keep-count "2"
      [editor repeat-count]
      (update-count editor 2))
    (:keep-count "3"
      [editor repeat-count]
      (update-count editor 3))
    (:keep-count "4"
      [editor repeat-count]
      (update-count editor 4))
    (:keep-count "5"
      [editor repeat-count]
      (update-count editor 5))
    (:keep-count "6"
      [editor repeat-count]
      (update-count editor 6))
    (:keep-count "7"
      [editor repeat-count]
      (update-count editor 7))
    (:keep-count "8"
      [editor repeat-count]
      (update-count editor 8))
    (:keep-count "9"
      [editor repeat-count]
      (update-count editor 9))

    (":"
      [editor]
      (command-line-mode/enter editor))

    ("^"
      [editor]
      (let [position (index-of-first-non-blank (current-line editor))]
        (change-column editor (constantly position))))

    ("$"
      [editor]
      (let [buffer (e/current-buffer editor)
            [i j] (b/cursor buffer)
            line-length (count (b/line buffer i))
            j (max 0 (dec line-length))]
        (change-column editor (constantly j))))

    ("gg"
      [editor repeat-count]
      (let [buffer (e/current-buffer editor)
            specified-line (dec (or repeat-count 1))
            last-line (dec (b/line-count buffer))
            target-line (min specified-line last-line)
            target-column (index-of-first-non-blank (b/line buffer target-line))]
        (-> editor
            (e/change-line (constantly target-line))
            (change-column (constantly target-column)))))

    ("h"
      [editor]
      (change-column editor dec))

    ("i"
      [editor]
      (assoc editor :mode :insert))

    ("j"
      [editor]
      (e/change-line editor inc))

    ("k"
      [editor]
      (e/change-line editor dec))

    ("l"
      [editor]
      (change-column editor inc))

    ("x"
      [editor]
      (let [buffer (e/current-buffer editor)
            [i j] (b/cursor buffer)
            before-line (b/line buffer i)
            after-line (str
                         (.substring before-line 0 j)
                         (.substring before-line (inc j)))]
        (e/update-current-buffer editor
                                 (fn [buffer]
                                   (assoc-in buffer [:lines i] after-line)))))

    ("G"
      [editor repeat-count]
      (let [buffer (e/current-buffer editor)
            last-line (dec (b/line-count buffer))
            target-line (if repeat-count
                          (dec repeat-count)
                          last-line)
            target-column (index-of-first-non-blank (b/line buffer target-line))]
        (-> editor
            (e/change-line (constantly target-line))
            (change-column (constantly target-column)))))

    ("H"
      [editor repeat-count]
      (let [count (dec (or repeat-count 1))]
        (e/update-current-buffer editor #(b/cursor-to-top-of-viewport % count))))

    ("L"
      [editor repeat-count]
      (let [count (dec (or repeat-count 1))]
        (e/update-current-buffer editor #(b/cursor-to-bottom-of-viewport % count))))

    ("M"
      [editor]
      (e/update-current-buffer editor b/cursor-to-middle-of-viewport))

    ("<C-D>"
      [editor]
      (let [buffer (e/current-buffer editor)]
        (if (b/on-last-line? buffer)
          (e/beep editor)
          (e/update-current-buffer editor #(b/move-and-scroll-half-page % :down)))))

    ("<C-E>"
      [editor]
      (scroll editor inc))

    ("<C-U>"
      [editor]
      (let [buffer (e/current-buffer editor)
            [i] (b/cursor buffer)]
        (if (zero? i)
          (e/beep editor)
          (e/update-current-buffer editor #(b/move-and-scroll-half-page % :up)))))

    ("<C-Y>"
      [editor]
      (scroll editor dec))

    (:else
      [editor]
      (e/beep editor))))

(defmethod e/process :normal
  [editor event]
  (em/invoke-event-handler eventmap editor event))
