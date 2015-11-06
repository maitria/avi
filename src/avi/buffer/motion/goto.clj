(ns avi.buffer.motion.goto
  (:require [avi.buffer
              [locations :as l]
              [motion :as m]]
            [schema.core :as s]))

(defmulti magic-row-value
  (fn [buffer kind param]
    kind))

(defmethod magic-row-value :current
  [buffer _ offset]
  (+ (get-in buffer [:point 0]) (or offset 0)))

(defmethod magic-row-value :viewport-top
  [{:keys [viewport-top]} _ lines-below]
  (+ viewport-top (or lines-below 0)))

(defmethod magic-row-value :viewport-bottom
  [{:keys [lines viewport-top viewport-height]} _ count-from-bottom]
  (let [count-from-bottom (or count-from-bottom 0)
        bottom-of-viewport (dec (+ viewport-top viewport-height))
        bottom-of-file (dec (count lines))
        count-from-bottom-of-viewport (- bottom-of-viewport count-from-bottom)
        count-from-bottom-of-file (- bottom-of-file count-from-bottom)
        new-line (max viewport-top (min count-from-bottom-of-viewport count-from-bottom-of-file))]
    new-line))

(defmethod magic-row-value :viewport-middle
  [{top :viewport-top,
    height :viewport-height,
    :keys [lines],
    :as buffer} _ _]
  (let [middle-of-viewport (dec (+ top (quot height 2)))
        middle-of-file (quot (dec (count lines)) 2)
        middle (min middle-of-viewport middle-of-file)]
    middle))

(defmulti magic-column-value
  (fn [buffer kind row param]
    kind))

(defmethod magic-column-value :end-of-line
  [{:keys [lines]} _ row _]
  (max 0 (dec (count (get lines row)))))

(defmethod magic-column-value :first-non-blank
  [{:keys [lines]} _ row _]
  (let [string (get lines row)
        leading-space-count (count (re-find #"^\s*" string))
        all-spaces? (and (> leading-space-count 0)
                         (= leading-space-count (count string)))]
    (if all-spaces?
      (dec leading-space-count)
      leading-space-count)))

(defmethod magic-column-value :last-explicit
  [{:keys [lines last-explicit-j]} _ i _]
  (let [j last-explicit-j
        line-length (count (get lines i))
        j-not-after-end (min (dec line-length) j)
        j-within-line (max 0 j-not-after-end)]
    j-within-line))


(s/defmethod m/resolve-motion :goto :- l/Location
  [{:keys [lines] :as buffer} [_ [i j]]]
  (let [i (cond
            (number? i) i
            (map? i)    (magic-row-value buffer (first (keys i)) (first (vals i)))
            :else       (magic-row-value buffer i nil))
        i (-> i
            (min (dec (count lines)))
            (max 0))
        j (cond
            (number? j) j
            (map? j)    (magic-column-value buffer (first (keys j)) i (first (vals j)))
            :else       (magic-column-value buffer j i nil))]
    [i j]))
