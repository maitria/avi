(ns avi.color
  (:require [clojure.set :refer [map-invert]]))

(def color->number
  {:black 0
   :red 1
   :green 2
   :yellow 3
   :blue 4
   :magenta 5
   :cyan 6
   :white 7})

(def number->color
  (map-invert color->number))

(defn make-attributes
  [color background]
  (byte (bit-or (bit-shift-left (color->number color) 3)
                (color->number background))))

(defn- foreground-color
  [character-attributes]
  (number->color (bit-and 7 (bit-shift-right character-attributes 3))))

(defn- background-color
  [character-attributes]
  (number->color (bit-and 7 character-attributes)))

(defn attr-description
  [character-attributes]
  (let [fg-keyword (foreground-color character-attributes)
        bg-keyword (background-color character-attributes)]
    (cond
      (= [:white :black] [fg-keyword bg-keyword])
      []

      (= :black bg-keyword)
      [fg-keyword]

      :else
      [fg-keyword :on bg-keyword])))
