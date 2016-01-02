(ns avi.nfa
  (:require [clojure.set :as set]
            [schema.core :as s]))

(def StateNumber
  s/Int)

(def NFA
  {:start #{StateNumber}
   :accept #{StateNumber}
   :transitions {s/Any {StateNumber {StateNumber s/Any}}}})

(def MatchState
  {:nfa NFA
   :states {s/Int {:value s/Any}}
   :status (s/enum :pending :accept :reject)})

(defn- null-reducer
  [accumulator _]
  accumulator)

(defn- merge-transitions
  [& xs]
  (apply merge-with (partial merge-with into) xs))

(defn- mapcat-transitions
  [f xs]
  (->>
    (for [[value froms] xs
          [from tos] froms
          [to reducer] tos]
      (f value from to reducer))
    (reduce concat)
    (reduce
      (fn [xs [value from to reducer]]
        (assoc-in xs [value from to] reducer))
      {})))

(defn- states
  [nfa]
  (->> (for [[_ froms] (:transitions nfa)
             [from tos] froms
             [to _] tos]
         [from to])
    (apply concat)
    (into #{})))

(s/defn ^:private renumber :- [NFA]
  "Renumber the states in each NFA so that no two NFAs share a state number."
  [nfas :- [NFA]]
  (second
    (reduce
      (fn [[n done] nfa]
        (let [original (states nfa)
              mapping (zipmap original (map (partial + n) (range)))
              nfa' {:start (->> (:start nfa) (map mapping) (into #{}))
                    :accept (->> (:accept nfa) (map mapping) (into #{}))
                    :transitions (mapcat-transitions
                                   (fn [value from to reducer]
                                     [[value (mapping from) (mapping to) reducer]])
                                   (:transitions nfa))}]
          [(+ n (count mapping)) (conj done nfa')]))
      [0 []]
      nfas)))

(s/defn match :- NFA
  [value :- s/Any]
  {:start #{0}
   :accept #{1}
   :transitions {value {0 {1 null-reducer}}}})

(def any
  (match ::any))

(s/defn maybe :- NFA
  [nfa :- NFA]
  {:start (:start nfa)
   :accept (set/union (:start nfa) (:accept nfa))
   :transitions (:transitions nfa)})

(s/defn choice :- NFA
  ([a :- NFA]
   a)
  ([a :- NFA
    b :- NFA]
   (let [[a b] (renumber [a b])]
     {:start (set/union (:start a) (:start b))
      :accept (set/union (:accept a) (:accept b))
      :transitions (merge-transitions
                     (:transitions a)
                     (:transitions b))}))
  ([a :- NFA
    b :- NFA
    & cs :- [NFA]]
   (reduce choice (concat [a b] cs))))

(s/defn kleene :- NFA
  ([nfa :- NFA]
   {:start (:start nfa)
    :accept (:start nfa)

    ;; any transition which is x -> a, a ∈ accept, is replace with all
    ;; x -> s ∀ s ∈ start
    :transitions (mapcat-transitions
                   (fn [value from to reducer]
                     (if ((:accept nfa) to)
                       (for [s (:start nfa)]
                         [value from s reducer])
                       [[value from to reducer]]))
                   (:transitions nfa))}))

(s/defn chain :- NFA
  ([a :- NFA]
   a)
  ([a :- NFA
    b :- NFA]
   (let [[a b] (renumber [a b])]
     {:start (if (seq (set/intersection (:start a) (:accept a)))
               (set/union (:start a) (:start b))
               (:start a))
      :accept (:accept b)
      :transitions (mapcat-transitions
                     (fn [value from to reducer]
                       (concat
                         [[value from to reducer]]
                         (if ((:accept a) to)
                           (for [s (:start b)]
                             [value from s reducer]))))
                     (merge-transitions
                       (:transitions a)
                       (:transitions b)))}))
  ([a :- NFA
    b :- NFA
    & cs :- [NFA]]
   (reduce chain (concat [a b] cs))))

(s/defn lookahead :- NFA
  [a :- NFA]
  a)

(s/defn on :- NFA
  [nfa :- NFA f]
  (update-in
    nfa
    [:transitions]
    (partial
      mapcat-transitions
      (fn [value from to reducer]
        (if ((:accept nfa) to)
          [[value from to (fn [acc input]
                            (let [result (reducer acc input)]
                              (if (= result ::prune)
                                ::prune
                                (let [result-value (f (:value result) input)]
                                  (if (= result-value ::prune)
                                    ::prune
                                    (assoc result :value result-value))))))]]
          [[value from to reducer]])))))

(s/defn prune :- NFA
  [nfa :- NFA f]
  (on nfa (fn [v _]
            (if (f v)
              ::prune
              v))))

(defn- characterize
  [nfa sub-states]
  (cond
    (empty? sub-states)
    :reject

    (not (empty? (set/intersection (:accept nfa) (into #{} (keys sub-states)))))
    :accept

    :else               :pending))

(s/defn start :- MatchState
  [nfa :- NFA]
  (let [states (->> (:start nfa)
                 (map #(vector % {:value nil}))
                 (into {}))]
    {:nfa nfa
     :states states
     :status (characterize nfa states)}))

(s/defn accept? :- s/Bool
  [{:keys [status]} :- MatchState]
  (= status :accept))

(s/defn reject? :- s/Bool
  [{:keys [status]} :- MatchState]
  (= status :reject))

(s/defn accept-value :- s/Any
  [nfa :- NFA
   state :- MatchState]
  (->> state
    :states
    (filter (comp (:accept nfa) first))
    (map second)
    first
    :value))

(s/defn advance :- MatchState
  [{:keys [nfa states] :as state} :- MatchState
   input :- s/Any
   stream-mark :- s/Any]
  (let [states' (->> (for [[s targets] (concat
                                         (get-in nfa [:transitions ::any])
                                         (get-in nfa [:transitions input]))
                           :when (contains? states s)
                           :let [v (get states s)]
                           [s' reducer] targets
                           :let [v' (reducer v input)]
                           :when (not= v' ::prune)]
                       [s' v'])
                     (into {}))]
    (assoc state
           :states states'
           :status (characterize nfa states'))))
