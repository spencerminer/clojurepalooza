(ns gaia-project.gaia-project
  (:gen-class)
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

(declare next-turn)
(declare interpret-input)

(def default-init-player
  {:points 10
   :resources {:gold 15 :ore 5 :knowledge 4 :qic 0}
   :gaiaformers_available 0 :gaiaformers_out 0
   :mines_out 0 :trading_stations_out 0 :planetary_institutes_out 0
   :research_stations_out 0 :research_institutes_out 0
   :income_track {:gold [0 2 3 3]
                  :ore [1 1 1 0 1 1 1 0 1]
                  :knowledge [1 1 1 1 2]}
   :tech_tiles {}                                           ;; key: tile_name, value: :covered or :uncovered
   :federation_tokens {}                                    ;; key: token_name, value: :green or :gray
   :turn_power_thing nil
   :tech_track {:terraforming 1 :navigation 1 :qic 1
                :gaiaforming 1 :income 1 :science 1}})

(defn init-player [faction]
  (case faction
    :terrans
    (-> default-init-player
        (assoc :name "terrans" :qic 1 :gaiaformers_available 1)
        (assoc-in [:tech_track :gaiaforming] 2))

    :alien2
    (-> default-init-player
        (assoc :name "alien2" :knowledge 3)
        (assoc-in [:tech_track :science] 2))))

(defn init-players [factions]
  (into {} (map #(vector % (init-player %)) factions)))

(defn new-game [factions]
  {:players (init-players factions)
   :turn []
   :passed factions
   :round 0
   :board {}
   :round_scorers []                                        ;; Scorer protocol?
   :endgame_scorers #{}
   :turn_power_things {}                                    ;; key: thing name, value: accumulated gold
   })

(defn rotate-turn [{:keys [turn] :as game}]
  (assoc game :turn
              (conj (vec (drop 1 turn))
                    (first turn))))

(rotate-turn {:turn [1 2 3 4]})

(defn take-resource [rsc bldg player]
  (take (inc (bldg (second player)))
        (get-in player [1 :income_track rsc])))

(defn take-income [player]
  (-> player
      (update-in [1 :resources :ore] #(apply + %1 %2)
                 (take-resource :ore :mines_out player))
      (update-in [1 :resources :gold] #(apply + %1 %2)
                 (take-resource :gold :trading_stations_out player))
      (update-in [1 :resources :knowledge] #(apply + %1 %2)
                 (concat
                   (take-resource :knowledge :research_stations_out player)
                   (take-last (:research_institutes_out (second player))
                              (get-in player [1 :income_track :knowledge]))))))

(defn take-all-income [game]
  (assoc game :players
              (into {} (for [player (:players game)]
                         (take-income player)))))
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

(defn invalid-choice [msg game turn]
  (do
    (println msg)
    (interpret-input (read-line) game turn)))

(defn interpret-input [raw-input game this-turn]
  (let [s (str/split raw-input #" ")]
    (case (first s)
      "pass"
      (-> game
          (update :passed conj this-turn)
          (update :turn #(vec (drop 1 %))))

      "tech"
      (let [track (keyword (second s))]
        (cond
          (nil? (get-in game [:players this-turn :tech_track track]))
          (invalid-choice "That's not a tech track, try again"
                          game this-turn)

          (< (get-in game [:players this-turn :resources :knowledge]) 4)
          (invalid-choice "Insufficient knowledge, do something else"
                          game this-turn)

          (->> game
               :players
               (map #(get-in % [1 :tech_track track]))
               (filter #(= 6 %))
               seq)
          (invalid-choice (str "The top level of that track is already "
                               "occupied, do something else")
                          game this-turn)

          (and (= 5 (get-in game [:players this-turn :tech_track track]))
               (->> [:players this-turn :federation_tokens]
                    (get-in game)
                    (map second)
                    (filter #(= :green %))                  ; don't have city token
                    empty?))
          (invalid-choice (str "You need a green federation token to go to the "
                               "top, do something else")
                          game this-turn)

          :else
          (-> game
              (update-in [:players this-turn :tech_track track] inc)
              (update-in [:players this-turn :resources :knowledge] - 4)
              rotate-turn)))

      (invalid-choice "Invalid command. Try again"
                      game this-turn))))

(defn next-round [game]
  (if (= 6 (:round game))
    (println "Game over. Scores:" (score-game game))
    (-> game
        (update :round inc)
        ; update scores
        ; update board stuff
        (assoc :turn (:passed game))
        (assoc :passed [])
        ; everyone takes income
        take-all-income
        ((fn [g] (println g) g))
        next-turn)))

(defn next-turn [game]
  (if (empty? (:turn game))
    (do
      (println "Round over!")
      (next-round game))
    (let [this-turn (first (:turn game))]
      (pp/pprint (this-turn (:players game)))
      (println "It's the" this-turn "turn. What do you want to do?")
      (next-turn (interpret-input (read-line) game this-turn))
      ; rotate the turn vector, or remove this-turn and conj it to passed
      )
    ))

(defn -main [& players]
  (println "Hello people")
  (println "these are our players:" players)
  (next-round (new-game (mapv keyword players))))

(comment
  (def test-game (let [game (new-game [:terrans :alien2])]
                   (-> game
                       (assoc :turn (:passed game))
                       (assoc :passed []))))
  (def player {:terrans (init-player :terrans)})
  (def players (:players test-game))
  (pp/pprint test-game)

  (score-resources player)
  (update-in player [1 :points] +
             (int (/ (->> player second :resources vals (apply +))
                     3)))
  )
