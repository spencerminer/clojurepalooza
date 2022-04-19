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

(def all-vertical-horizontal-points
  (->> endpoints
       (filter (fn [[[x1 y1] [x2 y2]]]
                 (or (= x1 x2) (= y1 y2))))
       (map endpoints->points)
       (apply concat)))

(def dimension 1000)

(defn init-board [x]
  (vec (repeat x (vec (repeat x 0)))))

(defn inc-in [board [x y]]
  (update-in board [y x] inc))

(defn gen-final-board [points]
  (reduce (fn [board point]
            (inc-in board point))
          (init-board dimension)
          points))

(defn count-overlaps-slow [board]
  (->> board
       flatten
       (filter #(<= 2 %))
       count))

;; This is ~15 times faster for the 1000x1000 full board!!
(defn count-overlaps-fast [board]
  (->> board
       (map (partial filter #(<= 2 %)))
       flatten
       count))

;; This is ~2 times faster than the `fast` version!
(defn count-overlaps-faster [board]
  (->> board
       (map (partial filter #(<= 2 %)))
       (map count)
       (apply +)))

(def part-1 (-> all-vertical-horizontal-points
                gen-final-board
                count-overlaps-fast))
part-1  ;; => 4655

(def final-board (gen-final-board all-points))
(def part-2 (count-overlaps-fast final-board))
part-2  ;; => 20500

(comment
 (time (count-overlaps-slow final-board)) 450
 (time (count-overlaps-fast final-board)) 30
 (time (count-overlaps-faster final-board)) 18)
