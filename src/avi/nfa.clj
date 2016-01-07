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
  [nfa]
  (vec (concat
         [[:split 1 (+ 2 (count nfa))]]
         nfa
         [[:goto (- (inc (count nfa)))]])))

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
  [nfa [pc thread-state] input stream-mark consumed?]
  (let [[opcode a b] (get nfa pc)]
    (case opcode
      :split  (->> [a b]
                (mapcat #(advance* nfa [(+ pc %) thread-state] input stream-mark consumed?))
                vec)
      :goto   (recur nfa [(+ pc a) thread-state] input stream-mark consumed?)
      :match  (if consumed?
                [[pc thread-state]]
                (when (or (= a ::any) (= a input))
                  (recur nfa [(inc pc) (assoc thread-state :end stream-mark)] input stream-mark true)))
      :on     (recur nfa [(inc pc) (update-in thread-state [:value] a input)] input stream-mark consumed?)
      :prune  (when-not (a (:value thread-state))
                (recur nfa [(inc pc) thread-state] input stream-mark consumed?))
      nil     (when consumed?
                [[pc thread-state]]))))

(defn- make-state
  [nfa threads]
  (with-meta
    (merge
      (get threads (count nfa))
      {:threads threads
       :status (characterize nfa threads)})
    {:nfa nfa}))

(defn start
  [nfa]
  (make-state nfa
              (->> (advance* nfa [0 nil] nil nil true)
                (into {}))))

(defn advance
  [{:keys [threads] :as state} [stream-mark input]]
  (let [nfa (:nfa (meta state))
        threads' (->> threads
                   (mapcat #(advance* nfa % input stream-mark false))
                   (into {}))]
    (make-state nfa threads')))
