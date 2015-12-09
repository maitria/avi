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
  {""  {:operator :move-point}
   "d" {:operator :delete}})

(def motions
  '{"0"    {:kind :exclusive, :motion [:goto [:current 0]]}
    "^"    {:kind :exclusive, :motion [:goto [:current :first-non-blank]]}
    "$"    {:kind :inclusive, :motion [:goto [:current :end-of-line]]}
    "f<.>" {:kind :inclusive, :motion [:goto [:current [:to-next ?char]]]}
    "gg"   {:kind :linewise,  :motion [:goto [(?line 0) :first-non-blank]]}
    "h"    {:kind :exclusive, :motion [:goto [:current :left]]}
    "j"    {:kind :linewise,  :motion [:goto [:down :last-explicit]]}
    "k"    {:kind :linewise,  :motion [:goto [:up :last-explicit]]}
    "l"    {:kind :exclusive, :motion [:goto [:current [:right (?count 1)]]]}
    "t<.>" {:kind :inclusive, :motion [:goto [:current [:before-next ?char]]]}
    "w"    {:kind :exclusive, :motion [:word :start [:forward (?count 1)]]}
    "F<.>" {:kind :exclusive, :motion [:goto [:current [:to-previous ?char]]]}
    "G"    {:kind :linewise,  :motion [:goto [(?line :last) :first-non-blank]]}
    "H"    {:kind :linewise,  :motion [:goto [[:viewport-top (?line 0)] :first-non-blank]]}
    "L"    {:kind :linewise,  :motion [:goto [[:viewport-bottom (?line 0)] :first-non-blank]]}
    "M"    {:kind :linewise,  :motion [:goto [:viewport-middle :first-non-blank]]}
    "T<.>" {:kind :exclusive, :motion [:goto [:current [:after-previous ?char]]]} })

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

(defn- event-string-nfa
  [event-string]
  (->> event-string
    ev/events
    (map event-nfa)
    (apply nfa/chain)))

(defn- map->nfa
  [mappings]
  (->> mappings
    (map (fn [[event-string handler]]
           (let [handler (decorate-event-handler handler)]
             (nfa/on (event-string-nfa event-string)
                     (fn [v _]
                       (assoc v :handler handler))))))
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
  (let [default-operator-spec (get operators "")]
    (->> motions
      (map (fn [[event-string spec]]
             (nfa/on (event-string-nfa event-string)
                     (fn motion-reducer [v _]
                       (merge
                         default-operator-spec
                         v
                         {:auto-repeat? (not (uses-count? (:motion spec)))
                          :handler motion-handler}
                         (update-in spec [:motion] substitute (bindings v)))))))
      (apply nfa/choice))))

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