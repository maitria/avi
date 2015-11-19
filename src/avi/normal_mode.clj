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
    {"0" (fn+> [editor _]
           (in e/current-buffer
             (b/move-point [:goto [:current 0]])))

     "^" (fn+> [editor _]
           (in e/current-buffer
             (b/move-point [:goto [:current :first-non-blank]])))

     "$" (fn+> [editor _]
           (in e/current-buffer
             (b/move-point [:goto [:current :end-of-line]])))

     "dd" ^:no-repeat (fn+> [editor _]
                        (let [repeat-count (:count editor)]
                          (in e/current-buffer
                              b/start-transaction
                              (n-times (or repeat-count 1) b/delete-current-line)
                              b/commit)))

     "f<.>" (fn+> [editor [_ key-name]]
              (let [ch (get key-name 0)]
                (in e/current-buffer
                  (b/move-point [:goto [:current [:to-char ch]]]))))

     "gg" ^:no-repeat (fn+> [editor _]
                        (let [repeat-count (:count editor)]
                          (in e/current-buffer
                            (b/move-point [:goto [(dec (or repeat-count 1)) :first-non-blank]]))))

     "h" (fn+> [editor _]
           (change-column dec))

     "j" (fn+> [editor _]
           (e/change-line inc))

     "k" (fn+> [editor _]
           (e/change-line dec))

     "l" (fn+> [editor _]
           (change-column inc))

     "t<.>" (fn+> [editor [_ key-name]]
              (let [ch (get key-name 0)]
                (in e/current-buffer
                  (b/move-point [:goto [:current [:before-next ch]]]))))

     "u" (fn+> [editor _]
           (in e/current-buffer
             b/undo))

     "x" ^:no-repeat (fn+> [editor _]
                       (let [repeat-count (:count editor)]
                         (in e/current-buffer
                             b/start-transaction
                             (as-> buffer
                               (reduce
                                 (fn [buffer n]
                                   (b/delete-char-under-point buffer))
                                 buffer
                                 (range (or repeat-count 1))))
                             b/commit)))

     "G" ^:no-repeat (fn+> [editor _]
                       (let [repeat-count (:count editor)]
                         (in e/current-buffer
                           (if repeat-count
                             (b/move-point [:goto [(dec repeat-count) :first-non-blank]])
                             (b/move-point [:goto [:last :first-non-blank]])))))

     "H" ^:no-repeat (fn+> [editor _]
                       (let [lines-from-top (dec (or (:count editor) 1))]
                         (in e/current-buffer
                           (b/move-point [:goto [[:viewport-top lines-from-top] :last-explicit]]))))

     "J" ^:no-repeat (fn+> [editor _]
                       (let [{[i j] :point, lines :lines} (e/current-buffer editor)
                             n (or (:count editor) 1)
                             new-line (reduce
                                        #(str %1 " " %2)
                                        (subvec lines i (+ i n 1)))
                             new-lines (splice lines i (+ i n 1) [new-line])]
                         (in e/current-buffer
                             b/start-transaction
                             (assoc :lines new-lines)
                             b/commit)))

     "L" ^:no-repeat (fn+> [editor event]
                       (let [count (dec (or (:count editor) 1))]
                         (in e/current-buffer
                           (b/move-point [:goto [[:viewport-bottom count] :last-explicit]]))))

     "M" (fn+> [editor _]
           (in e/current-buffer
             (b/move-point [:goto [:viewport-middle :last-explicit]])))

     "<C-D>" (fn+> [editor _]
               (let [buffer (e/current-buffer editor)]
                 (if (b/on-last-line? buffer)
                   beep/beep
                   (in e/current-buffer
                     (b/move-and-scroll-half-page :down)))))

     "<C-E>" (fn+> [editor _]
               (scroll inc))

     "<C-R>" (fn+> [editor _]
               (in e/current-buffer
                 b/redo))

     "<C-U>" (fn+> [editor _]
               (let [buffer (e/current-buffer editor)
                     [i] (:point buffer)]
                 (if (zero? i)
                   beep/beep
                   (in e/current-buffer
                     (b/move-and-scroll-half-page :up)))))

     "<C-Y>" (fn+> [editor _]
               (scroll dec))}))

(defn- update-count
  [editor digit]
  (let [old-count (or (:count editor) 0)
        new-count (+ (* 10 old-count) digit)]
    (assoc editor :count new-count)))

(defn- wrap-collect-repeat-count
  [responder]
  (fn+> [editor [event-type event-data :as event]]
    (if (seq (:pending-events editor))
      (responder event)
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
  (-> beep/beep-responder
      wrap-normal-mode
      avi.search/wrap-normal-search-commands
      command-line-mode/wrap-enter-command-line-mode
      insert-mode/wrap-enter-insert-mode
      brackets/wrap-go-to-matching-bracket
      wrap-collect-repeat-count))

(def wrap-mode (e/mode-middleware :normal responder))
