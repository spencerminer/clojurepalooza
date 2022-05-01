(ns advent-of-code-2021.day9
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(def heights (->> (util/get-lines "day9")
                  (map #(string/split % #""))
                  (mapv (fn [row]
                          (mapv parse-long row)))))

(def max-y (count heights))
(def max-x (count (first heights)))
(def all-points
  (for [x (range max-x)
        y (range max-y)]
    [x y]))

(defn get-value [board [x y]]
  (get-in board [y x]))

(defn set-value [board [x y] v]
  (assoc-in board [y x] v))

(defn get-filtered-neighbors [board [x y] pred]
  (filter (fn [point]
            (when-let [value (get-value board point)]
              (pred value)))
          [[(dec x) y]
           [(inc x) y]
           [x (dec y)]
           [x (inc y)]]))

(defn get-neighbor-values [board point]
  (map (partial get-value board)
       (get-filtered-neighbors board point (constantly true))))

;; This saves a get-in, but costs a (remove nil?) and messiness
;; Saving 1 get-in is good, but each get-neighbors has 4, so this isn't significant
#_(defn get-low-point-values [board]
  (->> all-points
       (map (fn [point]
              (let [value (get-value board point)]
                (when (->> point
                           (get-neighbor-values board)
                           (every? (partial < value)))
                  value))))
       (remove nil?)))

(defn get-low-points [board]
  (filter (fn [point]
            (let [value (get-value board point)]
              (->> point
                   (get-neighbor-values board)
                   (every? (partial < value)))))
          all-points))

(def part-1 (->> heights
                 get-low-points
                 (map (partial get-value heights))
                 (map inc)
                 (apply +)))
part-1  ;; => 417


(def already-searched 10)
(def unsearched? (set (range 9)))

(defn search-basin [floor-map search-queue current-size]
  (if-let [current-point (first search-queue)]
    (search-basin (set-value floor-map
                             current-point
                             already-searched)
                  (-> floor-map
                      (get-filtered-neighbors current-point unsearched?)
                      (concat (drop 1 search-queue))
                      set)
                  (inc current-size))
    [floor-map current-size]))

(defn find-basin-size [floor-map starting-point]
  (search-basin
   (set-value floor-map starting-point already-searched)
   (-> floor-map
       (get-filtered-neighbors starting-point unsearched?)
       set)
   1))

(def part-2
  (->> (reduce (fn [{:keys [floor-map basin-sizes] :as acc} current-point]
                 (if-not (unsearched? (get-value floor-map current-point))
                   acc
                   (let [[new-floor-map basin-size]
                         (find-basin-size floor-map current-point)]
                     {:floor-map new-floor-map
                      :basin-sizes (conj basin-sizes basin-size)})))
               {:floor-map heights
                :basin-sizes (list)}
               all-points)
       :basin-sizes
       sort
       (take-last 3)
       (apply *)))
part-2  ;; => 1148965

(deftest answers-still-correct-test
  (testing "For refactoring, you know"
    (is (= 1148965 part-2))
    (is (= 417 part-1))))

(comment
 [[4 2 1 0 1 2 * * * 8]
  [4 3 2 1 2 * 8 * 8 7]
  [5 7 3 4 * 8 7 7 * 4]
  [7 6 4 * 8 7 6 5 4 3]
  [8 7 5 6 * 8 * 6 5 2]
  [* 8 * 7 8 * 8 7 7 3]
  [5 * * 8 * * * 7 6 5]
  [4 5 6 * 8 * * 8 7 5]
  [3 4 5 6 7 8 * * 7 6]
  [2 3 4 8 * * * * 8 7]]
 (def small-board (mapv (comp vec
                              (partial take 10))
                        (take 10 heights)))
 (second (find-basin-size small-board [2 2]))

 (:basin-sizes
  (reduce (fn [{:keys [floor-map basin-sizes] :as acc} current-point]
            (if (#{9 already-searched} (get-value floor-map current-point))
              acc
              (let [[new-floor-map basin-size] (find-basin-size floor-map current-point)]
                (-> acc
                    (assoc :floor-map new-floor-map)
                    (update :basin-sizes conj basin-size)))))
          {:floor-map small-board
           :basin-sizes (list)}
          (shuffle (for [x (range 10)
                         y (range 10)]
                     [x y]))))
 )
