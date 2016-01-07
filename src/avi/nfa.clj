(ns avi.nfa
  "Regular expression matching using a virtual machine approach (see:
  https://swtch.com/~rsc/regexp/regexp2.html).  Abstract: Regular expressions
  are compiled into serieses of VM 'instructions' which are executed on a
  pseudo-multithreaded VM.

  Noteworthy aspects of this implementation:

  * This implementation uses a 'lockstep' method.  For each input value, we
    run every thread until it blocks needing more input, then suspend it.

  * The following properties hold for all produced VM programs:

    1. The only start state is instruction 0
    2. There is no ':accept' opcode.  Accepting is implicit - it happens by
       executing the instruction one past the end of the program.
    3. The addresses used by ':goto' and ':split' are relative instruction
       counts.

    The result is that programs can be sensically concatenated (see `choice'),
    and sophisticated programs can be built up from simple ones.

  * Each VM 'instruction' is a vector where the first element, the opcode, is
    a keyword.  Other elements are parameters.  See `advance*` for implemented
    opcodes.

  * Thread priorities are not implemented.

  * We are pruning duplicate threads pretty late.  It might be possible to
    produce programs that hang or overflow the stack.")

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
      :split  (->> [a b]
                (mapcat #(advance* nfa [(+ pc %) value] input consumed?))
                vec)
      :goto   (recur nfa [(+ pc a) value] input consumed?)
      :match  (if consumed?
                [[pc value]]
                (when (or (= a ::any) (= a input))
                  (recur nfa [(inc pc) value] input true)))
      :on     (recur nfa [(inc pc) (update-in value [:value] a input)] input consumed?)
      :prune  (when-not (a (:value value))
                (recur nfa [(inc pc) value] input consumed?))
      nil     (when consumed?
                [[pc value]]))))

(defn start
  [nfa]
  (let [threads (->> (advance* nfa [0 nil] nil true)
                  (into {}))]
    (with-meta
      {:threads threads
       :status (characterize nfa threads)}
      {:nfa nfa})))

(defn advance
  [{:keys [threads] :as state} [stream-mark input]]
  (let [nfa (:nfa (meta state))
        threads' (->> (for [[pc value] threads
                            [pc value] (advance* nfa [pc value] input false)]
                        [pc value])
                  (into {}))
        status (characterize nfa threads')]
    (with-meta
      {:threads threads'
       :status status
       :value (get-in threads' [(count nfa) :value])}
      {:nfa nfa})))
