(ns advent-of-code-2021.day2
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as string]))

(def directions (->> "day2"
                     util/get-lines
                     (map #(string/split % #" "))))

(def aggregated-directions
  (group-by first directions))

(def summed-directions
  (update-vals aggregated-directions
               (fn [direction-list]
                 (->> direction-list
                      (map second)
                      (map parse-long)
                      (apply +)))))

(def final-location-pt1
  {:forward (get summed-directions "forward")
   :down (- (get summed-directions "down")
            (get summed-directions "up"))})

(def part-1 (apply * (vals final-location-pt1)))
part-1  ;; => 1990000

(def final-location-pt2
  (reduce (fn [{:keys [aim] :as acc} [direction magnitude-str]]
            (let [magnitude (parse-long magnitude-str)]
              (case direction
                "down" (update acc :aim #(+ % magnitude))
                "up" (update acc :aim #(- % magnitude))
                "forward" (-> acc
                              (update :forward #(+ % magnitude))
                              (update :depth #(+ % (* aim magnitude)))))))
          {:forward 0
           :depth 0
           :aim 0}
          directions))

(def part-2 (* (:forward final-location-pt2)
               (:depth final-location-pt2)))
part-2  ;; => 1975421260
