(ns avi.edit-context.operate.goto
  (:require [avi.edit-context
              [locations :as l]]
            [avi.edit-context.operate.resolve :as resolve]
            [avi.pervasive :refer :all]))

(defmulti magic-row-value
  (fn [edit-context kind param]
    kind))

(defmethod magic-row-value :current
  [edit-context _ offset]
  (+ (get-in edit-context [:point 0]) (or offset 0)))

(defmulti magic-column-value
  (fn [edit-context kind row param]
    kind))

(defmethod magic-column-value :end-of-line
  [{:keys [:avi.documents/lines]} _ row _]
  (bit-shift-right Long/MAX_VALUE 1))

(defn first-non-blank
  [{:keys [:avi.documents/lines]} row]
  (let [string (get lines row)
        leading-space-count (count (re-find #"^\s*" string))
        all-spaces? (and (> leading-space-count 0)
                         (= leading-space-count (count string)))]
    (if all-spaces?
      (dec leading-space-count)
      leading-space-count)))

(defmethod magic-column-value :first-non-blank
  [edit-context _ row _]
  (first-non-blank edit-context row))

(defn next-char-index
  [{:keys [:avi.documents/lines]} [i j] direction ch]
  (let [line (get lines i)]
    (loop [nj (+ j direction)]
      (if-let [line-ch (get line nj)]
        (if (= line-ch ch)
          nj
          (recur (+ nj direction)))
        nil))))

(defn- clamp-point-row
  [{:keys [:avi.documents/lines]} row]
  (max 0 (min (dec (count lines)) row)))

(defn- absolutize
  [v f]
  (cond
    (number? v) v
    (coll? v)   (f (first v) (second v))
    :else       (f v nil)))

(defmethod resolve/resolve-motion :goto
  [{:keys [:avi.documents/lines] [i j] :point :as edit-context} {[_ [goto-i goto-j]] :motion}]
  (if-let [new-i (some->> (absolutize goto-i #(magic-row-value edit-context %1 %2))
                   (clamp-point-row edit-context))]
    (if-not goto-j
      [[i j] [new-i]]
      (let [new-j (absolutize goto-j #(magic-column-value edit-context %1 new-i %2))]
        (if new-j
          [[i j] [new-i new-j]])))))

(defmethod resolve/resolve-motion :down
  [{:keys [:avi.documents/lines] [i j] :point :as edit-context} {:keys [count]}]
  (let [new-i (+ i (or count 1))
        clamped-i (clamp-point-row edit-context new-i)]
    (if (get lines (inc i))
      [[i j] [clamped-i]])))

(defmethod resolve/resolve-motion :up
  [{[i j] :point :as edit-context} {:keys [count]}]
  (let [new-i (clamp-point-row edit-context (- i (or count 1)))]
    (if-not (neg? (dec i))
      [[i j] [new-i]])))

(defmethod resolve/resolve-motion :right
  [{:keys [:avi.documents/lines] [i j] :point} {:keys [operator] n :count}]
  (let [column (+ j (or n 1))
        max-j (cond-> (count (get lines i))
                (= :move-point operator)
                dec)
        column (max 0 (min column max-j))]
    (if-not (= j column)
      [[i j] [i column]])))

(defmethod resolve/resolve-motion :left
  [{:keys [:avi.documents/lines] [i j] :point} {n :count}]
  (let [column (max 0 (- j (or n 1)))]
    (if-not (= j column)
      [[i j] [i column]])))

(defmethod resolve/resolve-motion :goto-line
  [{:keys [:avi.documents/lines viewport-top viewport-height] [si sj] :point :as edit-context}
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
        i (clamp-point-row edit-context i)]
    [[si sj] [i (first-non-blank edit-context i)]]))

(defmethod resolve/resolve-motion :move-to-char
  [{[i j] :point :as edit-context} 
   {ch :char
    n :count
    [_ {:keys [direction offset]
        :or {direction +1
             offset 0}}] :motion}]
  (if-let [new-j (n-times
                   j
                   (or n 1)
                   (fn [j]
                     (if j
                       (some-> (next-char-index edit-context [i j] direction ch)
                          (+ offset)))))]
    [[i j] [i new-j]]))
