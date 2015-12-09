(ns avi.mode.normal
  (:require [packthread.core :refer :all]
            [avi.beep :as beep]
            [avi.brackets :as brackets]
            [avi.buffer :as b]
            [avi.editor :as e]
            [avi.events :as ev]
            [avi.mode command-line insert]
            [avi.nfa :as nfa]
            [avi.pervasive :refer :all]
            [avi.search]))

(def operators
  [[""  {:operator :move-point}]
   ["d" {:operator :delete}]])

(def motions
  '[["0"    :exclusive [:goto [:current 0]]]
    ["^"    :exclusive [:goto [:current :first-non-blank]]]
    ["$"    :inclusive [:goto [:current :end-of-line]]]
    ["f<.>" :inclusive [:goto [:current [:to-next ?char]]]]
    ["gg"   :linewise  [:goto [(?line 0) :first-non-blank]]]
    ["h"    :exclusive [:goto [:current :left]]]
    ["j"    :linewise  [:goto [:down :last-explicit]]]
    ["k"    :linewise  [:goto [:up :last-explicit]]]
    ["l"    :exclusive [:goto [:current :right]]]
    ["t<.>" :inclusive [:goto [:current [:before-next ?char]]]]
    ["w"    :exclusive [:word :start [:forward (?count 1)]]]
    ["F<.>" :exclusive [:goto [:current [:to-previous ?char]]]]
    ["G"    :linewise  [:goto [(?line :last) :first-non-blank]]]
    ["H"    :linewise  [:goto [[:viewport-top (?line 0)] :first-non-blank]]]
    ["L"    :linewise  [:goto [[:viewport-bottom (?line 0)] :first-non-blank]]]
    ["M"    :linewise  [:goto [:viewport-middle :first-non-blank]]]
    ["T<.>" :exclusive [:goto [:current [:after-previous ?char]]]]])

(defn bindings
  [spec]
  {'?char (:char spec)
   '?count (:count spec)
   '?line (some-> (:count spec) dec)})

(def count-variables
  #{'?count '?line})

(defn uses-count?
  [pattern]
  (->> pattern
    flatten
    (filter count-variables)
    seq))

(defn substitute
  [a bindings]
  (cond
    (contains? bindings a)
    (bindings a)

    (and (list? a) (contains? bindings (first a)))
    (if-let [value (bindings (first a))]
      value
      (second a))

    (coll? a)
    (into (empty a) (map #(substitute % bindings) a))

    :else
    a))

(defn motion-handler
  [editor {:keys [count auto-repeat?] :as spec}]
  (+> editor
    (in e/current-buffer
      (if auto-repeat?
        (n-times (or count 1) #(b/invoke-motion % spec))
        (b/invoke-motion spec)))))

(def non-motion-commands
  {"dd" ^:no-repeat (fn+> [editor spec]
                      (let [repeat-count (:count spec)]
                        (in e/current-buffer
                            b/start-transaction
                            (n-times (or repeat-count 1) b/delete-current-line)
                            b/commit)))

   "u" (fn+> [editor _]
         (in e/current-buffer
           b/undo))

   "x" ^:no-repeat (fn+> [editor spec]
                     (let [repeat-count (:count spec)]
                       (in e/current-buffer
                           b/start-transaction
                           (as-> buffer
                             (reduce
                               (fn [buffer n]
                                 (b/delete-char-under-point buffer))
                               buffer
                               (range (or repeat-count 1))))
                           b/commit)))
   "D" (fn+> [editor _]
         (in e/current-buffer
           (b/delete [:goto [:current :end-of-line]] :inclusive)))

   "J" ^:no-repeat (fn+> [editor spec]
                     (let [{[i j] :point, lines :lines} (e/current-buffer editor)
                           n (or (:count spec) 1)
                           new-line (reduce
                                      #(str %1 " " %2)
                                      (subvec lines i (+ i n 1)))
                           new-lines (splice lines i (+ i n 1) [new-line])]
                       (in e/current-buffer
                           b/start-transaction
                           (assoc :lines new-lines)
                           b/commit)))

   "<C-D>" (fn+> [editor _]
             (let [buffer (e/current-buffer editor)]
               (if (b/on-last-line? buffer)
                 beep/beep
                 (in e/current-buffer
                   (b/move-and-scroll-half-page :down)))))

   "<C-E>" (fn+> [editor _]
             (in e/current-buffer
               (b/scroll inc)))

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
             (in e/current-buffer
               (b/scroll dec)))})

(defn wrap-handler-with-repeat-loop
  [handler]
  (fn [editor spec]
    (let [repeat-count (or (:count spec) 1)]
      (nth (iterate #(handler % spec) editor) repeat-count))))

(defn decorate-event-handler
  [f]
  (+> f
    (if-not (:no-repeat (meta f))
      wrap-handler-with-repeat-loop)))

(defn- event-nfa
  [event]
  (cond
    (= [:keystroke "<.>"] event)
    (nfa/on nfa/any (fn [v [_ key-string]]
                      (assoc v :char (get key-string 0))))

    ;; It's not the "0" motion if we have a count (it's
    ;; part of the count).
    (= [:keystroke "0"] event)
    (nfa/prune (nfa/match [:keystroke "0"]) :count)

    :else
    (nfa/match event)))

(defn- map->nfa
  [mappings]
  (->> mappings
    (map (fn [[event-string handler]]
           (let [handler (decorate-event-handler handler)]
             (->> event-string
                  ev/events
                  (map event-nfa)
                  (apply nfa/chain)
                  (#(nfa/on % (fn [v _]
                                (assoc v :handler handler))))))))
    (apply nfa/choice)))

(def operator-nfa
  (nfa/maybe
    (->> operators
      (filter (fn [[prefix _]]
                (not= "" prefix)))
      (map (fn [[prefix spec]]
             (nfa/on (nfa/match [:keystroke prefix])
                     (fn [v _]
                       (merge v spec)))))
      (apply nfa/choice))))

(def motion-nfa
  (->> motions
    (map (fn [[event-string kind pattern]]
           (nfa/on (->> event-string
                     ev/events
                     (map event-nfa)
                     (apply nfa/chain))
                   (let [auto-repeat? (not (uses-count? pattern))
                         default-operator-spec (second (first operators))]
                     (fn motion-reducer [v _]
                       (merge
                         default-operator-spec
                         v
                         {:auto-repeat? auto-repeat?
                          :handler motion-handler
                          :kind kind
                          :motion (substitute pattern (bindings v))}))))))
    (apply nfa/choice)))

(defn count-digits-nfa
  [from to]
  (nfa/on (->> (range from (inc to))
            (map str)
            (map (partial vector :keystroke))
            (map nfa/match)
            (apply nfa/choice))
          (fn [v [_ key-string]]
            (let [old-value (or (:count v) 0)
                  new-value (+ (* 10 old-value)
                               (Integer/parseInt key-string))]
              (assoc v :count new-value)))))

(def count-nfa
  (nfa/maybe
    (nfa/chain
      (count-digits-nfa 1 9)
      (nfa/kleene
        (count-digits-nfa 0 9)))))

(def normal-nfa
  (nfa/chain
    count-nfa
    (nfa/choice
      (nfa/chain (nfa/maybe operator-nfa) motion-nfa)
      (map->nfa non-motion-commands)
      (map->nfa avi.mode.command-line/normal-commands)
      (map->nfa avi.search/normal-search-commands)
      (map->nfa brackets/normal-commands)
      (map->nfa avi.mode.insert/mappings-which-enter-insert-mode))))

(defn normal-responder
  [editor event]
  (+> editor
    (dissoc :normal-state)
    (let [state (or (:normal-state editor) (nfa/start normal-nfa))
          state' (nfa/advance normal-nfa state event :reject)]
      (cond
        (= state' :reject)
        beep/beep

        (nfa/accept? normal-nfa state')
        (let [value (nfa/accept-value normal-nfa state')]
          ((:handler value) value))

        :else
        (assoc :normal-state state')))))

(def wrap-mode (e/mode-middleware :normal normal-responder))
