(ns advent-of-code-2021.day5
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as string]))

(def raw-lines (util/get-lines "day5"))

(defn parse-line [line]
  (mapv #(mapv parse-long (string/split % #","))
        (string/split line #" -> ")))

(def endpoints (mapv parse-line raw-lines))

(defn spread [n1 n2]
  (let [step (if (< n1 n2) 1 -1)
        f (if (< n1 n2) inc dec)]
    (range n1 (f n2) step)))

(defn repeat-if-single [points]
  (if (= 1 (count points))
    (repeat (first points))
    points))

(defn endpoints->points [[[x1 y1] [x2 y2]]]
  (mapv vector
        (repeat-if-single (spread x1 x2))
        (repeat-if-single (spread y1 y2))))

(def all-points (->> endpoints
                     (map endpoints->points)
                     (apply concat)))

(def dimension 1000)

(defn init-board [x]
  (vec (repeat x (vec (repeat x 0)))))

(defn inc-in [board [x y]]
  (update-in board [y x] inc))

(def final-board
  (reduce (fn [board point]
            (inc-in board point))
          (init-board dimension)
          all-points))

"Ugh I have to only do vertical & horizontal lines rn"

(->> final-board
     (take 30)
     (map (partial take 30))   ;; see this
     flatten
     (apply +))
