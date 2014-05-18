(ns avi.pervasive)

(defprotocol Spliceable
  (splice [coll start end] [coll start end replacements]
    "Replace elements in coll from start (inclusive) to end (exclusive)."))

(extend-protocol Spliceable
  String
  (splice
    ([s start end]
     (str (.substring s 0 start)
          (.substring s end)))
    ([s start end substr]
     (str (.substring s 0 start)
          substr
          (.substring s end)))))
