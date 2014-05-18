(ns avi.pervasive)

(defn substring
  ([s start]
   (.substring s start))
  ([s start end]
   (.substring s start end)))

(defn splice
  "Replace or delete elements from the start index (inclusive) through the
  end index (exclusive).

  It is not an error for end to be well past the end of the collection. 
  "
  ([coll start end]
   (splice coll start end (empty coll)))
  ([coll start end replacements]
   (let [is-string? (string? coll)
         sub (if is-string? substring subvec)
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
