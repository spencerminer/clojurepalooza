(ns advent-of-code-2021.day6
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as string]))

(def starting-fish (mapv parse-long
                         (-> (util/get-lines "day6")
                             first
                             (string/split #","))))
(def time-to-spawn 7)
(def baby-time-to-spawn 9)

(defn next-gen [fishes]
  (let [num-spawners (count (filter zero? fishes))]
    (->> fishes
         (concat (repeat num-spawners baby-time-to-spawn))
         (replace {0 time-to-spawn})
         (mapv dec))))

(def part-1 (count (nth (iterate next-gen starting-fish) 80)))
part-1  ;; => 345387

;;(def part-2 (count (nth (iterate next-gen starting-fish) 256)))
;;part-2  ;; Execution error (OutOfMemoryError) at advent-of-code-2021.day6/next-gen (day6.clj:17).
          ;; GC overhead limit exceeded

;; Okay I might need to do some actual math...
;;(time (count (nth (iterate next-gen [0]) 256)))
;; that's still too long

(sort (zipmap (range)
              (take 30 (iterate next-gen [0]))))
