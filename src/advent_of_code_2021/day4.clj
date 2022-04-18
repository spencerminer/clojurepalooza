(ns advent-of-code-2021.day4
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as string]))

(def lines (util/get-lines "day4"))
(def all-numbers (mapv parse-long (string/split (first lines) #",")))
(def all-boards-unparsed (->> lines
                              rest
                              (partition-by #{""})
                              (remove #{(list "")})))

(defn parse-row [row]
  (mapv parse-long
        (-> row string/trim (string/split #"\s+"))))

(def all-boards
  (mapv (fn [board]
          (mapv parse-row board))
        all-boards-unparsed))

(def done :x)
(defn x-num-from-board [n board]
  (mapv (partial replace {n done})
        board))

(x-num-from-board 40 [[84 94 24 52 44]
                      [96 33 74 35 13]
                      [60 51 41 19 95]
                      [50 93 27 40 1]
                      [67 23 37 88 85]])
;;(mapv (partial x-num-from-board 27) (take 5 all-boards))

(defn transpose [vec-of-vecs]
  (apply mapv vector vec-of-vecs))

(defn won? [board]
  (->> board
       (concat (transpose board))
       (some #(every? #{done} %))))

(won? [[20 62 :x 36 12]
       [3  10 :x 8 56]
       [78 61 8 37 89]
       [72 26 :x 65 22]
       [:x 91 :x 5 63]])

(defn sum-board [board]
  (->> board
       flatten
       (remove #{done})
       (apply +)))

;; part-1
(loop [numbers all-numbers
       boards all-boards]
  (let [current-number (first numbers)
        next-boards (mapv (partial x-num-from-board current-number) boards)]
    (if-let [winning-boards (seq (filter won? next-boards))]
      (* (sum-board winning-boards) current-number)
      (recur (rest numbers) next-boards))))
;; => 22680

;; part-2
(loop [numbers all-numbers
       boards all-boards]
  (let [current-number (first numbers)
        xed-boards (mapv (partial x-num-from-board current-number) boards)
        winning-boards (seq (filter won? xed-boards))
        next-boards (remove (set winning-boards) xed-boards)]
    (if (zero? (count next-boards))
      (* (sum-board winning-boards) current-number)
      (recur (rest numbers) next-boards))))
;; => 16168
