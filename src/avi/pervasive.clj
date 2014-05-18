(ns avi.pervasive)

(defn splice
  "Replace or delete elements from start (inclusive) through end (exclusive)."
  ([coll start end]
   (splice coll start end (empty coll)))
  ([coll start end replacements]
   (if (string? coll)
     (str (.substring coll 0 start)
          replacements
          (.substring coll end))
     (vec (concat (subvec coll 0 start)
                  replacements
                  (subvec coll end))))))
