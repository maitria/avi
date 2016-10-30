(ns avi.mode.normal
  (:require [packthread.core :refer :all]
            [avi.beep :as beep]
            [avi.brackets :as brackets]
            [avi.edit-context :as ec]
            [avi.editor :as e]
            [avi.events :as ev]
            [avi.layout.panes :as p]
            [avi.mode command-line insert]
            [avi.nfa :as nfa]
            [avi.pervasive :refer :all]
            [avi.search]))

(def operators
  {""  {:operator :move-point}
   "d" {:operator :delete}})

(def motions
  '{"<Down>"{:span :linewise,
             :motion [:down]}
    "<Left>"{:span :exclusive,
             :motion [:left]}
    "<Right>"{:span :exclusive,
              :motion [:right]}
    "<Up>" {:span :linewise,
            :motion [:up]}
    "0"    {:span :exclusive,
            :motion [:goto [:current 0]]}
    "^"    {:span :exclusive,
            :motion [:goto [:current :first-non-blank]]}
    "$"    {:span :inclusive,
            :motion [:goto [:current :end-of-line]]}
    "b"    {:span :exclusive
            :motion [:word {:direction :backward
                            :position-in-word :start
                            :empty-lines? true}]}
    "e"    {:span :inclusive
            :motion [:word {:direction :forward
                            :position-in-word :end
                            :empty-lines? false}]}
    "f<.>" {:span :inclusive,
            :motion [:move-to-char]}
    "ge"   {:span :inclusive,
            :motion [:word {:direction :backward
                            :position-in-word :end
                            :empty-lines? true}]}
    "gg"   {:span :linewise,
            :motion [:goto-line {:default-line 0}]}
    "gE"   {:span :inclusive,
            :motion [:word {:direction :backward
                            :position-in-word :end
                            :big? true
                            :empty-lines? true}]}
    "h"    {:span :exclusive,
            :motion [:left]}
    "iw"   {:span :inclusive,
            :motion [:in-word]}
    "j"    {:span :linewise,
            :motion [:down]}
    "k"    {:span :linewise,
            :motion [:up]}
    "l"    {:span :exclusive,
            :motion [:right]}
    "t<.>" {:span :inclusive,
            :motion [:move-to-char {:offset -1}]}
    "w"    {:span :exclusive,
            :motion [:word {:weird-delete-clip? true
                            :direction :forward
                            :position-in-word :start
                            :empty-lines? true}]}
    "E"    {:span :inclusive
            :motion [:word {:direction :forward
                            :position-in-word :end
                            :big? true
                            :empty-lines? false}]}
    "F<.>" {:span :exclusive,
            :motion [:move-to-char {:direction -1}]}
    "G"    {:span :linewise,
            :motion [:goto-line {:default-line :last}]}
    "H"    {:span :linewise,
            :motion [:goto-line {:from :viewport-top}]}
    "L"    {:span :linewise,
            :motion [:goto-line {:from :viewport-bottom
                                 :multiplier -1}]}
    "M"    {:span :linewise,
            :motion [:goto-line {:from :viewport-middle
                                 :multiplier 0}]}
    "T<.>" {:span :exclusive,
            :motion [:move-to-char {:direction -1
                                    :offset 1}]}
    "W"    {:span :exclusive,
            :motion [:word {:direction :forward
                            :position-in-word :start
                            :big? true
                            :empty-lines? true}]}})

(defn motion-handler
  [editor spec]
  (+> editor
    (in e/edit-context
      (ec/operate spec))))

(def non-motion-commands
  {"dd" ^:no-repeat (fn+> [editor spec]
                      (let [repeat-count (:count spec)]
                        (in e/edit-context
                            ec/start-transaction
                            (n-times (or repeat-count 1) ec/delete-current-line)
                            ec/commit)))

   "u" (fn+> [editor _]
         (in e/edit-context
           ec/undo))

   "x" ^:no-repeat (fn+> [editor spec]
                     (in e/edit-context
                       (ec/operate (merge
                                     spec
                                     {:operator :delete
                                      :span :exclusive
                                      :motion [:right]}))))

   "D" (fn+> [editor _]
         (in e/edit-context
           (ec/operate {:operator :delete
                        :span :inclusive
                        :motion [:goto [:current :end-of-line]]})))

   "J" ^:no-repeat (fn+> [editor spec]
                     (let [n (or (:count spec) 2)]
                       (in e/edit-context
                           ec/start-transaction
                           (n-times (dec n) (fn+> [{:keys [:avi.documents/lines] [i] :point}]
                                              (let [start-j (count (get lines i))]
                                                (ec/change [i start-j] [(inc i) 0] " " :left)
                                                (ec/operate {:operator :move-point
                                                             :motion [:goto [i start-j]]}))))
                           ec/commit)))

   "<C-D>" (fn+> [editor _]
             (let [edit-context (e/edit-context editor)]
               (if (ec/on-last-line? edit-context)
                 beep/beep
                 (in e/edit-context
                   (ec/move-and-scroll-half-page :down)))))

   "<C-E>" (fn+> [editor _]
             (in e/edit-context
               (ec/scroll inc)))

   "<C-G>" (fn+> [editor _]
            (let [edit-context (e/edit-context editor)
                  [i] (:point edit-context)
                  document (get-in editor (e/current-document-path editor))
                  num-lines (count (:avi.documents/lines document))
                  line-no (inc i)
                  msg-txt (str "\"" (or (:avi.documents/name document) "[No Name]") "\"\t")]
              (if (not= (:avi.documents/lines document) [""])
                (assoc :message [:white :blue (str msg-txt num-lines " lines --" (int (/ (* line-no 100) num-lines)) "%--")])
                (assoc :message [:white :blue (str msg-txt "--No lines in buffer--")]))))

   "<C-R>" (fn+> [editor _]
             (in e/edit-context
               ec/redo))

   "<C-U>" (fn+> [editor _]
             (let [edit-context (e/edit-context editor)
                   [i] (:point edit-context)]
               (if (zero? i)
                 beep/beep
                 (in e/edit-context
                   (ec/move-and-scroll-half-page :up)))))

   "<C-W>h" (fn+> [editor _] (p/move [0 -1]))
   "<C-W>j" (fn+> [editor _] (p/move [+1 0]))
   "<C-W>k" (fn+> [editor _] (p/move [-1 0]))
   "<C-W>l" (fn+> [editor _] (p/move [0 +1]))

   "<C-Y>" (fn+> [editor _]
             (in e/edit-context
               (ec/scroll dec)))})

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
                         {:handler motion-handler}
                         spec)))))
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
          state' (nfa/advance state [nil event])]
      (cond
        (nfa/reject? state')
        beep/beep

        (nfa/accept? state')
        (let [value (nfa/accept-value state')]
          ((:handler value) value))

        :else
        (assoc :normal-state state')))))

(def wrap-mode (e/mode-middleware :normal normal-responder))
