(ns avi.string)

(defn index-of-first-non-blank
  [line]
  (let [leading-space-count (count (re-find #"^\s*" line))
        all-spaces? (and (> leading-space-count 0)
                         (= leading-space-count (count line)))]
    (if all-spaces?
      (dec leading-space-count)
      leading-space-count)))

