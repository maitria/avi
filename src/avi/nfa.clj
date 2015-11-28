(ns avi.nfa
  (:require [clojure.set :as set]))

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
          [to reducers] tos
          reducer reducers]
      (f value from to reducer))
    (reduce concat)
    (reduce
      (fn [xs [value from to reducer]]
        (update-in xs [value from to] conj reducer))
      {})))

(defn match
  ([value]
   (match value null-reducer))
  ([value reducer]
   (let [s1 (gensym)
         s2 (gensym)]
     {:start #{s1}
      :accept #{s2}
      :transitions {value {s1 {s2 [reducer]}}}})))

(defn any
  ([]
   (match ::any null-reducer))
  ([reducer]
   (match ::any reducer)))

(defn maybe
  [nfa]
  {:start (:start nfa)
   :accept (set/union (:start nfa) (:accept nfa))
   :transitions (:transitions nfa)})

(defn choice
  ([a]
   a)
  ([a b]
   {:start (set/union (:start a) (:start b))
    :accept (set/union (:accept a) (:accept b))
    :transitions (merge-transitions
                   (:transitions a)
                   (:transitions b))})
  ([a b & cs]
   (reduce choice (concat [a b] cs))))

(defn kleene
  ([nfa]
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

(defn chain
  ([a]
   a)
  ([a b]
   {:start (:start a)
    :accept (:accept b)
    :transitions (mapcat-transitions
                   (fn [value from to reducer]
                     (if ((:accept a) to)
                       (for [s (:start b)]
                         [value from s reducer])
                       [[value from to reducer]]))
                   (merge-transitions
                     (:transitions a)
                     (:transitions b)))})
  ([a b & cs]
   (reduce chain (concat [a b] cs))))

(defn start
  [nfa]
  (->> (:start nfa)
    (map #(vector % nil))
    (into {})))

(defn accept?
  [nfa state]
  (not (empty? (set/intersection
                 (:accept nfa)
                 (into #{} (keys state))))))

(defn accept-value
  [nfa state]
  (->> state
    (filter (comp (:accept nfa) first))
    (map second)
    first))

(defn advance
  [nfa state input reject-value]
  (let [state' (->> (for [[s targets] (concat
                                        (get-in nfa [:transitions ::any])
                                        (get-in nfa [:transitions input]))
                          :when (contains? state s)
                          :let [v (get state s)]
                          [s' reducers] targets
                          reducer reducers
                          :let [v' (reducer v input)]]
                      [s' v'])
                    (into {}))]
    (if (empty? state')
      reject-value
      state')))
