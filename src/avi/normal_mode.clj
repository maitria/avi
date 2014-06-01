(ns avi.normal-mode
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]
            [avi.command-line-mode :as command-line-mode]
            [avi.editor :as e]
            [avi.eventmap :as em]
            [avi.string :as s]))

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
  (+> editor
      (let [[i j] (:cursor (e/current-buffer editor))
        j (j-fn j)
        new-position [i j]]
        (if (cursor-can-move-to-column? editor new-position)
          (in e/current-buffer
              (b/move-cursor new-position j))
          e/beep))))

(defn- update-count
  [editor digit]
  (let [old-count (or (:count editor) 0)
        new-count (+ (* 10 old-count) digit)]
    (assoc editor :count new-count)))

(defn- current-line 
  [editor] 
  (let [buffer (e/current-buffer editor)
        [row] (:cursor buffer)]
    (b/line buffer row)))

(defn- scroll
  [editor update-fn]
  (+> editor
    (in e/current-buffer
        (b/scroll update-fn))))

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
      (let [position (s/index-of-first-non-blank (current-line editor))]
        (change-column editor (constantly position))))

    ("$"
      [editor]
      (let [buffer (e/current-buffer editor)
            [i j] (:cursor buffer)
            line-length (count (b/line buffer i))
            j (max 0 (dec line-length))]
        (change-column editor (constantly j))))

    ("a"
      [editor]
      (+> editor
          (assoc :mode :insert)
          (in e/current-buffer
            (let [{[i j] :cursor} (e/current-buffer editor)]
              (assoc :cursor [i (inc j)])))))

    ("dd"
      [editor]
      (+> editor
          (in e/current-buffer
              b/delete-current-line)))

    ("gg"
      [editor repeat-count]
      (let [buffer (e/current-buffer editor)
            specified-line (dec (or repeat-count 1))
            last-line (dec (b/line-count buffer))
            target-line (min specified-line last-line)
            target-column (s/index-of-first-non-blank (b/line buffer target-line))]
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

    ("o"
      [editor]
      (+> editor
          (let [{[i] :cursor} (e/current-buffer editor)]
            (in e/current-buffer
                (b/insert-blank-line (inc i)))
            (e/change-line inc)
            (assoc :mode :insert))))

    ("x"
      [editor]
      (+> editor
          (in e/current-buffer
              b/delete-char-under-cursor)))

    ("G"
      [editor repeat-count]
      (+> editor
          (let [buffer (e/current-buffer editor)
                last-line (dec (b/line-count buffer))
                target-line (if repeat-count
                              (dec repeat-count)
                              last-line)
                target-column (s/index-of-first-non-blank (b/line buffer target-line))]
            (e/change-line (constantly target-line))
            (change-column (constantly target-column)))))

    ("H"
      [editor repeat-count]
      (+> editor
          (let [count (dec (or repeat-count 1))]
            (in e/current-buffer
                (b/cursor-to-top-of-viewport count)))))

    ("L"
      [editor repeat-count]
      (+> editor
          (let [count (dec (or repeat-count 1))]
            (in e/current-buffer
                (b/cursor-to-bottom-of-viewport count)))))

    ("M"
      [editor]
      (+> editor
          (in e/current-buffer
              b/cursor-to-middle-of-viewport)))

    ("O"
      [editor]
      (+> editor
          (let [{[i] :cursor} (e/current-buffer editor)]
            (in e/current-buffer
                (b/insert-blank-line i))
            (assoc :mode :insert))))

    ("<C-D>"
      [editor]
      (+> editor
          (let [buffer (e/current-buffer editor)]
            (if (b/on-last-line? buffer)
              e/beep
              (in e/current-buffer
                  (b/move-and-scroll-half-page :down))))))

    ("<C-E>"
      [editor]
      (scroll editor inc))

    ("<C-U>"
      [editor]
      (+> editor
          (let [buffer (e/current-buffer editor)
                [i] (:cursor buffer)]
            (if (zero? i)
              e/beep
              (in e/current-buffer
                  (b/move-and-scroll-half-page :up))))))

    ("<C-Y>"
      [editor]
      (scroll editor dec))

    (:else
      [editor]
      (e/beep editor))))

(defmethod e/respond :normal
  [editor event]
  (em/invoke-event-handler eventmap editor event))
