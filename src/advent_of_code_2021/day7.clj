(ns advent-of-code-2021.day7
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as string]))

(def initial-positions (mapv parse-long
                             (-> (util/get-lines "day7")
                                 first
                                 (string/split #","))))

(def max-position (apply max initial-positions))  ;; => 1899

(comment
 (int                            ;; average starting position => 465
  (/ (apply + initial-positions)
     (count initial-positions)))
 (total-fuel-cost initial-positions 465)  ;;=> 364981
 (total-fuel-cost initial-positions 365)  ;;=> 349311
 )

(defn total-fuel-cost [initial-positions cost-fn destination]
  (->> initial-positions
       (map (partial cost-fn destination))
       (apply +)))

(defn calc-min-cost [initial-positions max-position cost-fn]
  (->> (range max-position)
       (map (partial total-fuel-cost initial-positions cost-fn))
       (apply min)))

(def part-1 (calc-min-cost initial-positions
                           max-position
                           (comp abs -)))
part-1  ;; => 347011     so much brute force


(defn individual-fuel-cost [a b]
  (apply + (range (inc (abs (- a b))))))

;; This is like 60 times faster
(defn individual-fuel-cost-mathy [a b]
  (let [diff (abs (- a b))]
    (/ (* diff (inc diff))
       2)))

(def part-2 (calc-min-cost initial-positions
                           max-position
                           individual-fuel-cost-mathy))
part-2  ;; => 98363777
