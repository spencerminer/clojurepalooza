(ns gaia-project.game)

(defn return-after-print [r & s]
  (println (apply str s))
  r)

(defn max-building-count [type]
  (get {:mines 9
        :trading-stations 4
        :planetary-institutes 1
        :research-stations 3
        :research-institutes 2}
       type))

(defn resource->building [resource]
  (get {:ore :mine
        :gold :trading-station
        :knowledge :research-station}
       resource))

(defn get-buildings-out [player type]
  (get-in player [:buildings-out type]))

(defn inc-tech-track [player tech]
  (update-in player [:tech-track tech] inc))

(defn get-cost [player type]
  (get-in player [:building-cost type]))

(defn get-resource [player resource]
  (get-in player [:resources resource]))

(defn get-board-resource-income-track [player resource]
  (get-in player [:income-track resource]))

(def default-init-player
  {:points 10
   :resources {:gold 15 :ore 5 :knowledge 4 :qic 0}
   :gaiaformers {:gaiaformers-available 0 :gaiaformers-out 0}
   :buildings-out {:mine 0 :trading-station 0 :planetary-institute 0
                   :research-station 0 :research-institute 0}
   :building-cost {:mine {:gold 2 :ore 1}
                   :trading-station {:gold 6 :ore 2}        ;; Is adjacent discount always half?
                   :planetary-institute {:gold 6 :ore 4}
                   :research-station {:gold 5 :ore 2}
                   :research-institute {:gold 6 :ore 6}}
   :income-track {:gold [0 2 3 3 3]
                  :ore [1 1 1 0 1 1 1 0 1]
                  :knowledge [1 1 1 1 2]}
   :tech-tiles {}                                           ;; key: tile-name, value: :covered or :uncovered
   :federation-tokens {}                                    ;; key: token-name, value: :green or :gray
   :turn-power-thing nil
   :tech-track {:terraforming 1 :navigation 1 :qic 1
                :gaiaforming 1 :income 1 :science 1}})

(defn init-player [faction]
  (case faction
    :terrans
    (-> default-init-player
        (assoc :name (name faction) :qic 1 :gaiaformers-available 1)
        (inc-tech-track :gaiaforming))

    :alien2
    (-> default-init-player
        (assoc :name (name faction) :knowledge 3)
        (inc-tech-track :science))))

(defn init-players [factions]
  (->> factions
       (map (fn [faction]
              [faction (init-player faction)]))
       (into {})))

(defn new-game [factions]
  {:players (init-players factions)
   :turn factions                          ;;(shuffle factions)
   :passed []
   :round 0
   :board {}
   :round-scorers []                                        ;; Scorer protocol?
   :endgame-scorers #{}
   :turn-power-things {}                                    ;; key: thing name, value: accumulated gold
   })

(defn rotate-turn [{:keys [turn] :as game}]
  (assoc game :turn
         (conj (vec (drop 1 turn))
               (first turn))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PAYING FOR STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn enough-resource [player resource amt]
  (> amt (get-resource player resource)))

(defn spend-resource [player resource amt]
  (if (enough-resource player resource amt)
    (return-after-print player "Not enough " (name resource))
    (update-in player [:resources resource] - amt)))

(defn gain-resource [player resource amt]
  (spend-resource player resource (- amt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INCOME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sum-board-resource-income [player resource]
  (let [buildings-out (get-buildings-out player (resource->building resource))]
    (->> resource
         (get-board-resource-income-track player)
         (take (inc buildings-out))
         (apply +))))

(defn take-player-income [player]
  (-> player
      (gain-resource :ore (sum-board-resource-income player :ore))
      (gain-resource :gold (sum-board-resource-income player :gold))
      (gain-resource :knowledge (sum-board-resource-income player :knowledge))))

(defn take-income [game]
  (assoc game :players
         (into {} (for [[name info] (:players game)]
                    [name (take-player-income info)]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCORING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn score-resources [player]
  (update-in player [1 :points] +
             (int (/ (get-in player [1 :resources]) (->> player second :resources vals (apply +))
                     3))))

(defn score-game [game]
  (let [plus-resources
        (->> (for [player (:players game)]
               (score-resources player))
             (into {})
             (assoc game :players))

        final-game plus-resources]
    (map (fn [player] [(:name player) (:points player)])
         (vals (:players final-game)))))
