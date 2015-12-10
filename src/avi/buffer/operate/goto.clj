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
  (bit-shift-right Long/MAX_VALUE 1))

(defn first-non-blank
  [{:keys [lines]} row]
  (let [string (get lines row)
        leading-space-count (count (re-find #"^\s*" string))
        all-spaces? (and (> leading-space-count 0)
                         (= leading-space-count (count string)))]
    (if all-spaces?
      (dec leading-space-count)
      leading-space-count)))

(defmethod magic-column-value :first-non-blank
  [buffer _ row _]
  (first-non-blank buffer row))

(defmethod magic-column-value :left
  [{[_ j] :point} _ _ param]
  (let [result (- j (or param 1))]
    (if-not (neg? result)
      result)))

(defn next-char-index
  [{:keys [lines] [_ j] :point} i ch]
  (let [line (get lines i)]
    (loop [nj (inc j)]
      (if-let [line-ch (get line nj)]
        (if (= line-ch ch)
          nj
          (recur (inc nj)))
        nil))))

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

(defmethod resolve/resolve-motion :goto
  [{:keys [lines] :as buffer} {[_ [i j]] :motion}]
  (if-let [i (some->> (absolutize i #(magic-row-value buffer %1 %2))
               (clamp-point-row buffer))]
    (if-not j
      [i]
      (let [j (absolutize j #(magic-column-value buffer %1 i %2))]
        (if j
          [i j])))))

(defmethod resolve/resolve-motion :down
  [{:keys [lines] [i] :point :as buffer} {:keys [count]}]
  (let [new-i (+ i (or count 1))]
    (if (get lines new-i)
      [(clamp-point-row buffer new-i)])))

(defmethod resolve/resolve-motion :up
  [{[i] :point} {:keys [count]}]
  (let [result (- i (or count 1))]
    (if-not (neg? result)
      [result])))

(defmethod resolve/resolve-motion :right
  [{:keys [lines] [i j] :point} {n :count}]
  (let [column (+ j (or n 1))
        column (if (get-in lines [i column])
                 column
                 (max 0 (dec (count (get lines i)))))]
    (if-not (= j column)
      [i column])))

(defmethod resolve/resolve-motion :goto-line
  [{:keys [lines viewport-top viewport-height] :as buffer}
   {count-register :count,
    [_ {:keys [from default-line multiplier]
        :or {default-line 0
             multiplier 1}}] :motion}]
  (let [default-line (if (= default-line :last)
                       (dec (count lines))
                       default-line)
        i (or (some-> count-register dec (* multiplier)) default-line)
        i (case from
            :viewport-top    (+ i viewport-top)
            :viewport-bottom (max
                               viewport-top
                               (+ i (min (dec (+ viewport-top viewport-height))
                                         (dec (count lines)))))
            i)
        i (clamp-point-row buffer i)]
    [i (first-non-blank buffer i)]))

(defmethod resolve/resolve-motion :move-to-char
  [{[i] :point :as buffer} {:keys [char]
                            [_ {:keys [offset]
                                :or {offset 0}}] :motion}]
  (if-let [j (some-> (next-char-index buffer i char)
                (+ offset))]
    [i j]))
