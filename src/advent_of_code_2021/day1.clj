(ns advent-of-code-2021.day1
  (:require [advent-of-code-2021.util :as util]))

(def depths (->> "day1"
                 util/get-lines
                 (map parse-long)))

(defn count-depth-increases [depths]
  (->> depths
       (partition 2 1)
       (map (comp (partial apply -)
                  (juxt last first)))
       (filter pos?)
       count))

(def part-1 (count-depth-increases depths))
part-1 ;; => 1557

(def part-2
  (->> depths
       (partition 3 1)
       (map (partial apply +))
       count-depth-increases))
part-2 ;; => 1608


;; Full threaded versions for fun
(->> "day1"
     util/get-lines
     (map parse-long)
     (partition 2 1)
     (map (comp (partial apply -)
                (juxt last first)))
     (filter pos?)
     count)
;; => 1557

(->> "day1"
     util/get-lines
     (map parse-long)
     (partition 3 1)
     (map (partial apply +))
     (partition 2 1)
     (map (comp (partial apply -)
                (juxt last first)))
     (filter pos?)
     count)
;; => 1608
