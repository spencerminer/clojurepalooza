(ns clojurepalooza.gaia-project)

(def default-init-player
  {:points 10
   :gold 15 :ore 5 :knowledge 4 :qic 0
   :gaiaformers_available 0 :gaiaformers_out 0
   :mines_out 0 :trading_stations_out 0 :planetary_institutes_out 0
   :research_stations_out 0 :research_institutes_out 0
   :tech_tiles {}                                           ;; key: tile_name, value: :covered or :uncovered
   :federation_tokens {}                                    ;; key: token_name, value: :green or :gray
   :turn_power_thing nil
   :tech_track {:terraforming 1 :navigation 1 :qic 1
                :gaiaforming 1 :income 1 :science 1}})

(defn init-player [faction]
  (case faction
    :terrans
    (-> default-init-player
        (assoc :qic 1 :gaiaformers_available 1)
        (assoc-in [:tech_track :gaiaforming] 2))

    :alien2
    (-> default-init-player
        (assoc :knowledge 3)
        (assoc-in [:tech_track :science] 2))))

(defn init-players [factions]
  (mapv init-player factions))

(defn new-game [factions]
  {:players (init-players factions)
   :round 1
   :board {}
   :round_scorers []                   ;; Scorer protocol?
   :endgame_scorers #{}
   :turn_power_things {}               ;; key: thing name, value: accumulated gold
   })

(defn next-round [game]
  (if (= 6 (:round game))
    (println "Game over. Calculate scores!")
    (-> game
        (update :round inc)
        )))
