(ns avi.normal-mode
  (:require [packthread.core :refer :all]
            [avi.brackets :as brackets]
            [avi.buffer :as b]
            [avi.command-line-mode :as command-line-mode]
            [avi.editor :as e]
            [avi.eventmap :as em]
            [avi.insert-mode :as insert-mode]
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

(def wrap-normal-mode
  (em/eventmap
    ("0"
      [editor]
      (change-column editor (constantly 0)))

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

    ("j"
      [editor]
      (e/change-line editor inc))

    ("k"
      [editor]
      (e/change-line editor dec))

    ("l"
      [editor]
      (change-column editor inc))

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
      (scroll editor dec))))

(defn- update-count
  [editor digit]
  (let [old-count (or (:count editor) 0)
        new-count (+ (* 10 old-count) digit)]
    (assoc editor :count new-count)))

(defn- wrap-collect-repeat-count
  [responder]
  (fn [editor [event-type event-data :as event]]
    (+> editor
      (cond
        (= event [:keystroke "0"])
        (if (:count editor)
          (update-count 0)
          (responder event))

        (and (= 1 (count event-data))
             (Character/isDigit (get event-data 0)))
        (update-count (Integer/parseInt event-data))

        :else
        (responder event)))))

(def responder
  (-> em/beep-responder
      wrap-normal-mode
      command-line-mode/wrap-enter-command-line-mode
      insert-mode/wrap-enter-insert-mode
      brackets/wrap-go-to-matching-bracket
      wrap-collect-repeat-count
      insert-mode/wrap-insert-mode
      command-line-mode/wrap-command-line-mode
      e/wrap-handle-resize
      em/wrap-reset-beep))

(defmethod e/respond :normal
  [editor event]
  (responder editor event))
