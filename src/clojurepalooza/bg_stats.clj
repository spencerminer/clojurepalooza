(ns clojurepalooza.bg-stats
  (:require [cheshire.core :as json]))

(defn laitrap [f & last-args]
  (fn [first-arg]
    (apply f first-arg last-args)))

(def all-stats
  (-> "resources/BGStatsExport.json"
      slurp
      (json/parse-string true)))

(keys all-stats)
;; => (:challenges :userInfo :games :plays :locations :players)

(count (:games all-stats))         ;; => 90
(count (:plays all-stats))         ;; => 277
(count (:locations all-stats))     ;; => 12
(count (:players all-stats))       ;; => 62

(def id-game-map
  (->> all-stats
       :games
       (map (juxt :id :name))
       (into {})))

(def game-freqs
  (->> all-stats
       :plays
       (map :gameRefId)
       (map (partial get id-game-map))
       frequencies
       ;(sort-by second)
       ;reverse
       ))

(defn h-index [freqs]
  (for [n (map inc (range 30))]
    (->> freqs
         (filter
          (fn [[name play-count]]
            (>= play-count n)))
         count
         (vector n))))

(h-index game-freqs)

