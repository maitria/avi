(ns avi.nfa)

(defn match
  ([value]
   (match value (fn [a _] a)))
  ([value reducer]
   (let [s1 (gensym)
         s2 (gensym)]
     {:start #{s1}
      :accept #{s2}
      :transitions {value {s1 {s2 [reducer]}}}})))

(defn start
  [nfa]
  (->> (:start nfa)
    (map #(vector % nil))
    (into {})))

(defn accept?
  [nfa]
  false)

(defn advance
  [nfa state input reject-value]
  (let [state' (->> (for [x (concat
                              (get-in nfa [:transitions ::any])
                              (get-in nfa [:transitions input]))
                          [s v] state
                          :when (contains? x s)
                          :let [[s' reducers] (get x s)]
                          reducer reducers
                          :let [v' (reducer v)]]
                      [s' v'])
                    (into {}))]
    (if (empty? state')
      reject-value
      state')))
