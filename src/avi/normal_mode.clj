(ns avi.normal-mode
  (:require [packthread.core :refer :all]
            [avi.beep :as beep]
            [avi.brackets :as brackets]
            [avi.buffer :as b]
            [avi.command-line-mode :as command-line-mode]
            [avi.editor :as e]
            [avi.eventmap :as em]
            [avi.insert-mode :as insert-mode]
            [avi.pervasive :refer :all]
            [avi.search]))

(defn- change-column
  [editor j-fn]
  (+> editor
      (let [{[i j] :point, :as buffer} (e/current-buffer editor)
        j (j-fn j)
        new-position [i j]]
        (if (b/point-can-move-to-column? buffer j)
          (in e/current-buffer
              (b/move-point [:goto new-position]))
          beep/beep))))

(defn- current-line 
  [editor] 
  (let [buffer (e/current-buffer editor)
        [row] (:point buffer)]
    (b/line buffer row)))

(defn- scroll
  [editor update-fn]
  (+> editor
    (in e/current-buffer
        (b/scroll update-fn))))

(def wrap-normal-mode
  (em/eventmap
    {"0" (em/eventfn [editor]
           (+> editor
             (in e/current-buffer
               (b/move-point [:goto [:current 0]]))))

     "^" (em/eventfn [editor]
           (+> editor
             (in e/current-buffer
               (b/move-point [:goto [:current :first-non-blank]]))))

     "$" (em/eventfn [editor]
           (+> editor
             (in e/current-buffer
               (b/move-point [:goto [:current :end-of-line]]))))

     "dd" (em/eventfn [editor repeat-count]
            (+> editor
              (in e/current-buffer
                b/start-transaction
                (n-times (or repeat-count 1) b/delete-current-line)
                b/commit)))

     "f<.>" (em/eventfn [editor]
              (+> editor
                (let [ch (get (second (last (:pending-events editor))) 0)]
                  (in e/current-buffer
                    (b/move-point [:goto [:current [:to-char ch]]])))))

     "gg" (em/eventfn [editor repeat-count]
            (+> editor
              (in e/current-buffer
                (b/move-point [:goto [(dec (or repeat-count 1)) :first-non-blank]]))))

     "h" (em/eventfn [editor]
           (change-column editor dec))

     "j" (em/eventfn [editor]
           (e/change-line editor inc))

     "k" (em/eventfn [editor]
           (e/change-line editor dec))

     "l" (em/eventfn [editor]
           (change-column editor inc))

     "u" (em/eventfn [editor]
           (+> editor
             (in e/current-buffer
               b/undo)))

     "x" (em/eventfn [editor repeat-count]
           (+> editor
             (in e/current-buffer
               b/start-transaction
               (as-> buffer
                 (reduce
                   (fn [buffer n]
                     (b/delete-char-under-point buffer))
                   buffer
                   (range (or repeat-count 1))))
               b/commit)))

     "G" (em/eventfn [editor repeat-count]
           (+> editor
             (in e/current-buffer
               (if repeat-count
                 (b/move-point [:goto [(dec repeat-count) :first-non-blank]])
                 (b/move-point [:goto [:last :first-non-blank]])))))

     "H" (em/eventfn [editor repeat-count]
           (+> editor
             (let [lines-from-top (dec (or repeat-count 1))]
               (in e/current-buffer
                 (b/move-point [:goto [[:viewport-top lines-from-top] :last-explicit]])))))

     "J" (em/eventfn [editor repeat-count]
           (+> editor
             (let [{[i j] :point, lines :lines} (e/current-buffer editor)
                   n (or repeat-count 1)
                   new-line (reduce
                              #(str %1 " " %2)
                              (subvec lines i (+ i n 1)))
                   new-lines (splice lines i (+ i n 1) [new-line])]
               (in e/current-buffer
                 b/start-transaction
                 (assoc :lines new-lines)
                 b/commit))))

     "L" (em/eventfn [editor repeat-count]
           (+> editor
             (let [count (dec (or repeat-count 1))]
               (in e/current-buffer
                 (b/move-point [:goto [[:viewport-bottom count] :last-explicit]])))))

     "M" (em/eventfn [editor]
           (+> editor
             (in e/current-buffer
               (b/move-point [:goto [:viewport-middle :last-explicit]]))))

     "<C-D>" (em/eventfn [editor]
               (+> editor
                 (let [buffer (e/current-buffer editor)]
                   (if (b/on-last-line? buffer)
                     beep/beep
                     (in e/current-buffer
                       (b/move-and-scroll-half-page :down))))))

     "<C-E>" (em/eventfn [editor]
               (scroll editor inc))

     "<C-R>" (em/eventfn [editor]
               (+> editor
                 (in e/current-buffer
                   b/redo)))

     "<C-U>" (em/eventfn [editor]
               (+> editor
                 (let [buffer (e/current-buffer editor)
                       [i] (:point buffer)]
                   (if (zero? i)
                     beep/beep
                     (in e/current-buffer
                       (b/move-and-scroll-half-page :up))))))

     "<C-Y>" (em/eventfn [editor]
               (scroll editor dec))}))

(defn- update-count
  [editor digit]
  (let [old-count (or (:count editor) 0)
        new-count (+ (* 10 old-count) digit)]
    (assoc editor :count new-count)))

(defn- wrap-collect-repeat-count
  [responder]
  (fn+> [editor [event-type event-data :as event]]
    (cond
      (= event [:keystroke "0"])
      (if (:count editor)
        (update-count 0)
        (responder event))

      (and (= 1 (count event-data))
           (Character/isDigit (get event-data 0)))
      (update-count (Integer/parseInt event-data))

      :else
      (responder event))))

(def responder
  (-> beep/beep-responder
      wrap-normal-mode
      avi.search/wrap-normal-search-commands
      command-line-mode/wrap-enter-command-line-mode
      insert-mode/wrap-enter-insert-mode
      brackets/wrap-go-to-matching-bracket
      wrap-collect-repeat-count))

(def wrap-mode (e/mode-middleware :normal responder))
