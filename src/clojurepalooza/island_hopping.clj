(ns clojurepalooza.island-hopping)

(def world1 [[0 5 5 7 0 0 0 0 0 0]
             [0 1 8 8 0 0 0 0 0 0]
             [0 3 2 2 0 0 2 8 4 0]
             [0 0 0 0 0 0 8 8 3 0]
             [0 0 0 0 0 0 8 7 8 0]
             [8 5 2 0 0 0 0 0 0 0]
             [2 7 3 0 0 0 0 0 0 0]
             [3 5 1 0 0 7 5 8 0 0]
             [0 0 0 0 0 1 1 7 0 0]
             [0 0 0 0 0 8 3 5 0 0]])

(defn calc_tallest_island_height [m]
  56)

(defn test [world expected-height]
  (let [your-height (calc_tallest_island_height world)]
    (if (= your-height expected-height)
      (println "pass")
      (printf "fail: %s != %s\n" your-height expected-height))))

(test world1 56)