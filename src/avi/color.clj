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

(defn make
  [foreground background]
  (byte (bit-or (bit-shift-left (color->number foreground) 3)
                (color->number background))))

(defn- foreground-name
  [color]
  (number->color (bit-and 7 (bit-shift-right color 3))))

(defn- background-name
  [color]
  (number->color (bit-and 7 color)))

(defn description
  [color]
  (let [fg-keyword (foreground-name color)
        bg-keyword (background-name color)]
    (cond
      (= [:white :black] [fg-keyword bg-keyword])
      []

      (= :black bg-keyword)
      [fg-keyword]

      :else
      [fg-keyword :on bg-keyword])))
