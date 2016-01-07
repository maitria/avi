(ns avi.nfa)

(defn match
  [value]
  [[:match value]])

(def any
  [[:match ::any]])

(defn maybe
  [nfa]
  (into
    [[:split 1 (inc (count nfa))]]
    nfa))

(defn choice
  ([a]
   a)
  ([a b]
   (vec (concat
          [[:split 1 (+ 2 (count a))]]
          a
          [[:goto (inc (count b))]]
          b)))
  ([a b & cs]
   (reduce choice (concat [a b] cs))))

(defn kleene
  ([nfa]
   (vec (concat
          [[:split 1 (+ 2 (count nfa))]]
          nfa
          [[:goto (- (inc (count nfa)))]]))))

(defn chain
  [& nfas]
  (reduce into [] nfas))

(defn lookahead
  [a]
  a)

(defn on
  [nfa f]
  (vec (concat nfa [[:on f]])))

(defn prune
  [nfa f]
  (vec (concat nfa [[:prune f]])))

(defn- characterize
  [nfa threads]
  (cond
    (empty? threads)                :reject
    (contains? threads (count nfa)) :accept
    :else                           :pending))

(defn accept?
  [{:keys [status]}]
  (= status :accept))

(defn reject?
  [{:keys [status]}]
  (= status :reject))

(defn accept-value
  [state]
  (:value state))

(defn- advance*
  [nfa [pc value] input consumed?]
  (let [[opcode a b] (get nfa pc)]
    (case opcode
      :split
      (into [] (concat
                 (advance* nfa [(+ pc a) value] input consumed?)
                 (advance* nfa [(+ pc b) value] input consumed?)))

      :goto
      (recur nfa [(+ pc a) value] input consumed?)

      :match
      (if consumed?
        [[pc value]]
        (when (or (= a ::any) (= a input))
          (recur nfa [(inc pc) value] input true)))

      :on
      (recur nfa [(inc pc) (update-in value [:value] a input)] input consumed?)

      :prune
      (when-not (a (:value value))
        (recur nfa [(inc pc) value] input consumed?))

      nil
      (when consumed?
        [[pc value]]))))

(defn start
  [nfa]
  (let [threads (->> (advance* nfa [0 nil] nil true)
                  (into {}))]
    {:nfa nfa
     :threads threads
     :status (characterize nfa threads)}))

(defn advance
  [{:keys [nfa threads] :as state} [stream-mark input]]
  (let [threads' (->> (for [[pc value] threads
                            [pc value] (advance* nfa [pc value] input false)]
                        [pc value])
                  (into {}))
        status (characterize nfa threads')]
    (assoc state
           :threads threads'
           :status status
           :value (get-in threads' [(count nfa) :value]))))
