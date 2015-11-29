(ns avi.eventmap
  (:require [avi.beep :as beep]
            [avi.editor :as e]
            [avi.nfa :as nfa]
            [packthread.core :refer :all]))

(defn wrap-handler-with-repeat-loop
  [handler]
  (fn [editor event]
    (let [repeat-count (or (:count editor) 1)]
      (nth (iterate #(handler % event) editor) repeat-count))))

(defn wrap-handler-with-count-reset
  [handler]
  (fn [editor event]
    (-> editor
        (handler event)
        (assoc :count nil))))

(defn split-string-of-commands
  [key-sequence]
  (lazy-seq
    (if-let [[_ key rest] (re-matches #"^(<[^<]+>|[^<])(.*)$" (or key-sequence ""))]
      (cons key (split-string-of-commands rest)))))

(defn- events
  [string-of-commands]
  (->> (split-string-of-commands string-of-commands)
       (map #(vector :keystroke %))
       vec))

(defn decorate-event-handler
  [f]
  (+> f
    (if-not (:no-repeat (meta f))
      wrap-handler-with-repeat-loop)
    wrap-handler-with-count-reset))

(defn- eventmap->nfa
  [mappings]
  (->> mappings
    (map (fn [[event-string handler]]
           (->> event-string
                events
                (map (fn [ev]
                       (if (= [:keystroke "<.>"] ev)
                         nfa/any
                         (nfa/match ev))))
                (apply nfa/chain)
                (#(nfa/on % (constantly handler))))))
    (apply nfa/choice)))

(defn eventmap
  [mappings]
  (let [nfa (eventmap->nfa (map (juxt first (comp decorate-event-handler second)) mappings))]
    (fn [responder]
      (fn [editor event]
        (let [state (or (:eventmap-state editor) (nfa/start nfa))
              state' (nfa/advance nfa state event :reject)]
          (cond
            (= state' :reject)
            (+> editor
              (dissoc :eventmap-state)
              (responder event))

            (nfa/accept? nfa state')
            (+> editor
              ((nfa/accept-value nfa state') event)
              (dissoc :eventmap-state))

            :else
            (assoc editor :eventmap-state state')))))))
