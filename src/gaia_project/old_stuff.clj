(ns gaia-project.old-stuff
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))


(declare next-turn)
(declare interpret-input)

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
          (nil? (get-in game [:players this-turn :tech-track track]))
          (invalid-choice "That's not a tech track, try again"
                          game this-turn)

          (< (get-in game [:players this-turn :resources :knowledge]) 4)
          (invalid-choice "Insufficient knowledge, do something else"
                          game this-turn)

          (->> game
               :players
               (map #(get-in % [1 :tech-track track]))
               (filter #(= 6 %))
               seq)
          (invalid-choice (str "The top level of that track is already "
                               "occupied, do something else")
                          game this-turn)

          (and (= 5 (get-in game [:players this-turn :tech-track track]))
               (->> [:players this-turn :federation-tokens]
                    (get-in game)
                    (map second)
                    (filter #(= :green %))                  ; don't have city token
                    empty?))
          (invalid-choice (str "You need a green federation token to go to the "
                               "top, do something else")
                          game this-turn)

          :else
          (-> game
              (update-in [:players this-turn :tech-track track] inc)
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
