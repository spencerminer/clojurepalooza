(ns advent-of-code-2021.day3
  (:require [advent-of-code-2021.util :as util]
            [clojure.math :as math]
            [hashp.core]))

(def lines (util/get-lines "day3"))
(def line-len (count (first lines)))

(defn get-max-digit [coll]
  (let [freqs (frequencies coll)]
    (if (= (freqs \0) (freqs \1))
      \1
      (first (apply max-key second freqs)))))

(defn get-max-digit-for-col [lines col]
  (->> lines
       (map #(nth % col))
       get-max-digit))

(def gamma-chars
  (map (partial get-max-digit-for-col lines) (range line-len)))

(def flip-bit {0 1, \0 \1
               1 0, \1 \0})

(def epsilon-chars
  (map flip-bit gamma-chars))

;; I know I could do (Integer/parseInt "10110001" 2), but I wanted to write it
;; I also found that mine can handle larger numbers than Integer/parseInt!
(defn bin->dec [bin]
  (let [powers-of-2 (map (partial math/pow 2) (range))
        bin-digits (map (comp parse-long str) (reverse bin))]
    (apply + (map * powers-of-2 bin-digits))))

(def gamma-rate (bin->dec gamma-chars))
(def epsilon-rate (bin->dec epsilon-chars))

(def part-1 (int (* gamma-rate epsilon-rate)))
part-1  ;; => 3885894

(defn filter-by-max-from-col [[col lines]]
  (let [max-digit (get-max-digit-for-col lines col)]
    [(inc col)
     (filter (fn [row]
               (= max-digit (nth row col)))
             lines)]))

(defn filter-by-min-from-col [[col lines]]
  (if (= 1 (count lines))
    [(inc col) lines]
    (let [min-digit (flip-bit (get-max-digit-for-col lines col))]
      [(inc col)
       (filter (fn [row]
                 (= min-digit (nth row col)))
               lines)])))

(def oxy-gen-rating
  (-> (iterate filter-by-max-from-col [0 lines])
      (nth line-len)
      second
      first
      bin->dec))

(def co2-scrb-rating
  (-> (iterate filter-by-min-from-col [0 lines])
      (nth line-len)
      second
      first
      bin->dec))

(def part-2 (int (* oxy-gen-rating co2-scrb-rating)))
part-2  ;; => 4375225
