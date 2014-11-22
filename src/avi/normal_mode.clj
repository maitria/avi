(ns avi.normal-mode
  (:require [packthread.core :refer :all]
            [avi.brackets :as brackets]
            [avi.buffer :as b]
            [avi.command-line-mode :as command-line-mode]
            [avi.editor :as e]
            [avi.eventmap :as em]
            [avi.pervasive :refer :all]
            [avi.string :as s]))

(defn- change-column
  [editor j-fn]
  (+> editor
      (let [{[i j] :cursor, :as buffer} (e/current-buffer editor)
        j (j-fn j)
        new-position [i j]]
        (if (b/cursor-can-move-to-column? buffer j)
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
      (e/enter-mode :finished))

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
      (e/enter-mode editor :command-line))

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

    ("%"
      [editor]
      (+> editor
        (let [{[i j] :cursor, lines :lines} (e/current-buffer editor)
              new-cursor (brackets/matching-bracket [i j] lines)]
          (if new-cursor
            (in e/current-buffer
              (assoc :cursor new-cursor))
            e/beep))))

    ("a"
      [editor repeat-count]
      (+> editor
          (e/enter-mode :insert)
          (in e/current-buffer
            (let [{[i j] :cursor, lines :lines} (e/current-buffer editor)
                  new-j (min (count (get lines i)) (inc j))]
              (assoc :cursor [i new-j])))))

    ("dd"
      [editor repeat-count]
      (+> editor
          (in e/current-buffer
              b/start-transaction
              (n-times (or repeat-count 1) b/delete-current-line)
              b/commit)))

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
      [editor repeat-count]
      (e/enter-mode editor :insert))

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
      [editor repeat-count]
      (+> editor
          (let [{[i] :cursor} (e/current-buffer editor)]
            (e/enter-mode :insert :script-prefix [[:keystroke "<Enter>"]])
            (in e/current-buffer
                (b/insert-blank-line (inc i)))
            (e/change-line inc))))

    ("u"
      [editor]
      (+> editor
          (in e/current-buffer
              b/undo)))

    ("x"
      [editor repeat-count]
      (+> editor
          (in e/current-buffer
              b/start-transaction
              (as-> buffer
                (reduce
                  (fn [buffer n]
                    (b/delete-char-under-cursor buffer))
                  buffer
                  (range (or repeat-count 1))))
              b/commit)))

    ("A"
      [editor repeat-count]
      (+> editor
          (e/enter-mode :insert)
          (in e/current-buffer
            (let [{[i] :cursor, lines :lines} (e/current-buffer editor)
                  j (count (get lines i))]
              (assoc :cursor [i j])))))

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

    ("J"
      [editor repeat-count]
      (+> editor
        (let [{[i j] :cursor, lines :lines} (e/current-buffer editor)
              n (or repeat-count 1)
              new-line (reduce
                         #(str %1 " " %2)
                         (subvec lines i (+ i n 1)))
              new-lines (splice lines i (+ i n 1) [new-line])]
          (in e/current-buffer
            b/start-transaction
            (assoc :lines new-lines)
            b/commit))))

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
      [editor repeat-count]
      (+> editor
          (let [{[i] :cursor} (e/current-buffer editor)]
            (e/enter-mode :insert :script-prefix [[:keystroke "<Enter>"]])
            (in e/current-buffer
                (b/insert-blank-line i))
            (change-column (constantly 0)))))

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

    ("<C-R>"
      [editor]
      (+> editor
        (in e/current-buffer
            b/redo)))

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
  (eventmap editor event))