(ns avi.pervasive)

(defn splice
  "Replace or delete elements from the start index (inclusive) through the
  end index (exclusive).

  It is not an error for end to be well past the end of the collection."
  
  ([coll start end]
   (splice coll start end (empty coll)))
  ([coll start end replacements]
   (let [is-string? (string? coll)
         sub (if is-string? subs subvec)
         con (if is-string? str (comp vec concat))]
     (con (sub coll 0 start)
          replacements
          (if (>= end (count coll))
            (empty coll)
            (sub coll end))))))

(comment

  (= "xa" (splice "a" 0 0 "x"))
  (= "a" (splice "a" 1 42))
  (= [1] (splice [1] 1 42))

  )

(defn n-times
  [thing n a-fn]
  (reduce
    (fn [thing n]
      (a-fn thing))
    thing
    (range n)))

;; Not the right place for this...
(defn fail
  [& args]
  (let [options (into #{} args)
        beep? (:beep options)
        message (first (filter string? args))
        ex-map (cond-> {}
                 beep? (assoc :beep? true)
                 message (assoc :message [:white :red message]))]
    (throw (ex-info (or message "") ex-map))))

