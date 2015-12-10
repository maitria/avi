(ns avi.pervasive)

(defn n-times
  [thing n a-fn]
  (reduce
    (fn [thing n]
      (a-fn thing))
    thing
    (range n)))

(defn subs-with-spaces
  "Like subs, except that it is not an error to index past the end of the
  string.  If `start` is greater, we pretend that the string was longer.  If
  `end` is greater, we pretend as theough the string were padded with spaces."
  ([s start]
   {:pre (string? s)}
   (if (> start (count s))
     ""
     (subs s start)))
  ([s start end]
   {:pre [(string? s)]
    :post [(= (count %) (- end start))]}
   (let [s-start (min start (count s))
         s-end (min end (count s))]
     (apply str
            (subs s s-start s-end)
            (repeat (- end start (- s-end s-start)) \space)))))
