(ns avi.buffer.operate.goto
  (:require [avi.buffer
              [locations :as l]]
            [avi.buffer.operate.resolve :as resolve]
            [avi.pervasive :refer :all]
            [schema.core :as s]))

(defmulti magic-row-value
  (fn [buffer kind param]
    kind))

(defmethod magic-row-value :current
  [buffer _ offset]
  (+ (get-in buffer [:point 0]) (or offset 0)))

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

(defn next-char-index
  [{:keys [lines]} [i j] direction ch]
  (let [line (get lines i)]
    (loop [nj (+ j direction)]
      (if-let [line-ch (get line nj)]
        (if (= line-ch ch)
          nj
          (recur (+ nj direction)))
        nil))))

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
  (if-let [new-i (some->> (absolutize i #(magic-row-value buffer %1 %2))
                   (clamp-point-row buffer))]
    (if-not j
      [new-i]
      (let [j (absolutize j #(magic-column-value buffer %1 new-i %2))]
        (if j
          [new-i j])))))

(defmethod resolve/resolve-motion :down
  [{:keys [lines] [i] :point :as buffer} {:keys [count]}]
  (let [new-i (+ i (or count 1))
        clamped-i (clamp-point-row buffer new-i)]
    (if (get lines (inc i))
      [clamped-i])))

(defmethod resolve/resolve-motion :up
  [{[i] :point :as buffer} {:keys [count]}]
  (let [new-i (clamp-point-row buffer (- i (or count 1)))]
    (if-not (neg? (dec i))
      [new-i])))

(defmethod resolve/resolve-motion :right
  [{:keys [lines] [i j] :point} {:keys [operator] n :count}]
  (let [column (+ j (or n 1))
        max-j (cond-> (count (get lines i))
                (= :move-point operator)
                dec)
        column (max 0 (min column max-j))]
    (if-not (= j column)
      [i column])))

(defmethod resolve/resolve-motion :left
  [{:keys [lines] [i j] :point} {n :count}]
  (let [column (max 0 (- j (or n 1)))]
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
            :viewport-middle (let [middle-of-viewport (dec (+ viewport-top (quot viewport-height 2)))
                                   middle-of-file (quot (dec (count lines)) 2)
                                   middle (min middle-of-viewport middle-of-file)]
                               (+ i middle))
            i)
        i (clamp-point-row buffer i)]
    [i (first-non-blank buffer i)]))

(defmethod resolve/resolve-motion :move-to-char
  [{[i j] :point :as buffer} {ch :char
                            n :count
                            [_ {:keys [direction offset]
                                :or {direction +1
                                     offset 0}}] :motion}]
  (if-let [j (n-times
               j
               (or n 1)
               (fn [j]
                 (if j
                   (some-> (next-char-index buffer [i j] direction ch)
                      (+ offset)))))]
    [i j]))
