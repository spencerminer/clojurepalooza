(ns clojurepalooza.bg-stats
  (:require [cheshire.core :as json]
            [clojure.math :as math]))

(defn laitrap [f & last-args]
  (fn [first-arg]
    (apply f first-arg last-args)))

(def all-stats
  (-> "resources/BGStatsExport.json"
      slurp
      (json/parse-string true)))

(keys all-stats)
;; => (:groups :userInfo :players :locations :games :plays :tags :challenges)

(count (:games all-stats))                                  ;; => 177
(count (:plays all-stats))                                  ;; => 801
(count (:locations all-stats))                              ;; => 15
(count (:players all-stats))                                ;; => 101

(def id-game-map
  (->> all-stats
       :games
       (map (juxt :id :name))
       (into {})))

(defn avg [coll]
  (when (seq coll)
    (int (/ (apply + coll)
            (count coll)))))

(defn game-ref-id->name [game-ref-id]
  (get id-game-map game-ref-id))

(defn player-count [play]
  (count (:playerScores play)))

(defn average-duration [plays]
  (->> plays
       (remove #(zero? (:durationMin %)))
       (map :durationMin)
       avg))

(defn game-plays-stats [plays]
  {:average-time (average-duration plays)
   :play-count (count plays)
   :average-time-by-player-count (update-vals (group-by player-count plays)
                                              average-duration)
   :play-count-by-player-count (update-vals (group-by player-count plays)
                                            count)})

(def game-stats
  (->> all-stats
       :plays
       (group-by :gameRefId)
       ((fn [game-plays]
          (-> game-plays
              (update-keys game-ref-id->name)
              (update-vals game-plays-stats))))
       ;;(sort-by (comp :average-time second))
       ))

(defn round-decimals
  ([x]
   (round-decimals x 0))
  ([x decimal-places]
   (let [shifter (math/pow 10 decimal-places)]
     (-> x
         (* shifter)
         math/round
         (/ shifter)))))

(comment
 ;; top designers!
 (->> all-stats
      :games
      (map :designers)
      frequencies
      (sort-by second))

 (count game-stats)
 (->> game-stats
      (filter (fn [[_game-name stats]]
                (<= 2 (:play-count stats))))
      ;;(sort-by (comp :play-count second))
      ;;reverse
      (map (fn [[game-name stats]]
             [(-> stats
                  :average-time                             ;; some have avg time nil...
                  (/ 60)
                  (round-decimals 1))
              game-name]))
      (sort-by first)
      )

 ;; example play
 {:durationMin 210,
  :playerScores [{:winner false,
                  :seatOrder 0,
                  :score "120",
                  :startPlayer false,
                  :playerRefId 28,
                  :rank 0,
                  :newPlayer false}
                 {:winner true,
                  :seatOrder 0,
                  :score "165",
                  :startPlayer false,
                  :playerRefId 25,
                  :rank 0,
                  :newPlayer false}],
  :importPlayId 0,
  :scoringSetting 0,
  :gameRefId 23,
  :usesTeams false,
  :bggId 43149225,
  :playDate "2020-05-18 10:00:00",
  :nemestatsId 0,
  :modificationDate "2020-06-16 15:28:09",
  :entryDate "2020-05-18 01:28:29",
  :expansionPlays [],
  :locationRefId 3,
  :bggLastSync "2020-06-16 15:28:09",
  :manualWinner false,
  :playImages "[]",
  :uuid "e7f78289-a2d9-4d56-ba89-b94aef56c43f",
  :rounds 0,
  :rating 0,
  :ignored false}
 )

(def game-play-counts
  (->> all-stats
       :plays
       (map :gameRefId)
       (map (partial get id-game-map))
       frequencies
       ;;(sort-by second)
       ))

(defn h-index [freqs]
  (for [n (map inc (range 50))]
    (->> freqs
         (filter
          (fn [[name play-count]]
            (>= play-count n)))
         count
         (vector n))))

(h-index game-play-counts)
