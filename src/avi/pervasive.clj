(ns avi.pervasive)

(defn splice
  "Replace or delete elements starting at the start index, up to but not
  including the end index.

  It is not an error for end to be well past the end of the collection."
  {:test (fn test-splice []
           (assert (= "xa" (splice "a" 0 0 "x")) "splice inserts at 0")
           (assert (= "a" (splice "a" 1 42)))
           (assert (= [1] (splice [1] 1 42)))
           (let [check-splice (splice "xxxxxx" 2 4 "a")]
             (assert (= "xxaxx" check-splice) (str "actual result was " check-splice))))}
  ([collection start end]
   (splice collection start end (empty collection)))
  ([collection start end replacements]
   (let [subcollection (if (string? collection) subs subvec)
         vector-concatenate (comp vec concat)
         concatenate (if (string? collection) str vector-concatenate)
         first-section (subcollection collection 0 start)
         last-section (if (>= end (count collection))
                        (empty collection)
                        (subcollection collection end))]
     (concatenate
       first-section
       replacements
       last-section))))

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

