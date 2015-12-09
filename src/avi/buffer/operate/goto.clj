(ns avi.buffer.operate.goto
  (:require [avi.buffer
              [locations :as l]]
            [avi.buffer.operate.resolve :as resolve]
            [schema.core :as s]))

(defmulti magic-row-value
  (fn [buffer kind param]
    kind))

(defmethod magic-row-value :current
  [buffer _ offset]
  (+ (get-in buffer [:point 0]) (or offset 0)))

(defmethod magic-row-value :down
  [{:keys [lines] [i] :point} _ n]
  (let [result (+ i (or n 1))]
    (if (get lines result)
      result)))

(defmethod magic-row-value :up
  [{[i] :point} _ n]
  (let [result (- i (or n 1))]
    (if-not (neg? result)
      result)))

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

(defmethod magic-row-value :last
  [{:keys [lines]} _ _]
  (max 0 (dec (count lines))))

(defmulti magic-column-value
  (fn [buffer kind row param]
    kind))

(defmethod magic-column-value :end-of-line
  [{:keys [lines]} _ row _]
  (bit-shift-right Long/MAX_VALUE 1))

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

(defmethod magic-column-value :left
  [{[_ j] :point} _ _ param]
  (let [result (- j (or param 1))]
    (if-not (neg? result)
      result)))

(defmethod magic-column-value :right
  [{:keys [lines] [_ j] :point} _ i param]
  (let [column (+ j (or param 1))
        column (if (get-in lines [i column])
                 column
                 (max 0 (dec (count (get lines i)))))]
    (if-not (= j column)
      column)))

(defn next-char-index
  [{:keys [lines] [_ j] :point} i ch]
  (let [line (get lines i)]
    (loop [nj (inc j)]
      (if-let [line-ch (get line nj)]
        (if (= line-ch ch)
          nj
          (recur (inc nj)))
        nil))))

(defmethod magic-column-value :to-next
  [buffer _ i ch]
  (next-char-index buffer i ch))

(defmethod magic-column-value :before-next
  [buffer _ i ch]
  (some-> (next-char-index buffer i ch) dec))

(defn previous-char-index
  [{:keys [lines] [_ j] :point} i ch]
  (let [line (get lines i)]
    (loop [nj (dec j)]
      (if-let [line-ch (get line nj)]
        (if (= line-ch ch)
          nj
          (recur (dec nj)))
        nil))))

(defmethod magic-column-value :to-previous
  [buffer _ i ch]
  (previous-char-index buffer i ch))

(defmethod magic-column-value :after-previous
  [buffer _ i ch]
  (some-> (previous-char-index buffer i ch) inc))

(defn- clamp-point-row
  [{:keys [lines]} row]
  (max 0 (min (dec (count lines)) row)))

(defn- absolutize
  [v f]
  (cond
    (number? v) v
    (coll? v)   (f (first v) (second v))
    :else       (f v nil)))

(s/defmethod resolve/resolve-motion :goto :- (s/maybe l/Location)
  [{:keys [lines] :as buffer} [_ [i j]]]
  (if-let [i (absolutize i #(magic-row-value buffer %1 %2))]
    (let [i (clamp-point-row buffer i)
          j (absolutize j #(magic-column-value buffer %1 i %2))]
      (if j
        [i j]))))
