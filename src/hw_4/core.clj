(ns hw-4.core
  (:require [quil.core :as q]
            [clojure.math.combinatorics :as c]))

(def ^:const func #(q/cos (* %1 %2)))
(def ^:const field {:xs [-3 3] :ys [-3 3]})

(def ^:const grid-size-vertical   100)
(def ^:const grid-size-horizontal 1500)

(defn isometric-transformer [[x y]]
  (let [z (func x y)]
    [(/ (* (- y x) (q/sqrt 3)) 2)
     (- (/ (+ x y) 2) z)]))

(defn dimetric-transformer [[x y]]
  (let [z (func x y)]
    [(+ (/ (- x) (* 2 (q/sqrt 2))) y)
     (- (/ x (* 2 (q/sqrt 2))) z)]))

(defn recalc-coord [[min-val max-val] field-size val]
  (* field-size (/ (- val min-val) (- max-val min-val))))

(defn recalc-point [[min-max-x min-max-y] [x y]]
  [(q/floor (recalc-coord min-max-x (q/width) x))
   (q/floor (recalc-coord min-max-y (q/height) y))])

(defn transformed-with [transformer points]
  (let [points  (map transformer points)
        [xs ys] (apply mapv vector points)

        min-max-info [[(apply min xs) (apply max xs)]
                      [(apply min ys) (apply max ys)]]]

    (map (partial recalc-point min-max-info) points)))

(defn get-coord [[left right] grid-size idx]
  (+ right (/ (* idx (- left right)) (dec grid-size))))

(defn get-coords [field grid-size]
  (let [grid (range grid-size)]
    (map (partial get-coord field grid-size) grid)))

(def points
  (c/cartesian-product
    (get-coords (:xs field) grid-size-vertical)
    (get-coords (:ys field) grid-size-horizontal)))

(def points-rev
  (c/cartesian-product
    (get-coords (:xs field) grid-size-horizontal)
    (get-coords (:ys field) grid-size-vertical)))

(defn draw-points [points]
  (reduce
    (fn [[top bot] [x y]]
      (let [old-top (get top x (inc y))
            old-bot (get bot x (dec y))]

        (if (< y old-top)
          (do
            (q/stroke 255 80 80)
            (q/point x y))
          (when (> y old-bot)
            (q/stroke 80 80 180)
            (q/point x y)))

        [(assoc top x (min y old-top))
         (assoc bot x (max y old-bot))]))
    [{} {}]
    points))

(defn draw-transformed-with [transformer]
  (draw-points (transformed-with transformer points))
  (draw-points (transformed-with transformer points-rev)))

(defn draw []
  (q/background 255)
  (draw-transformed-with isometric-transformer))

(defn -main []
  (q/defsketch hw-4
    :size [800 600]
    :draw draw))
