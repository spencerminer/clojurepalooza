(ns gaia-project.gaia-project
  (:gen-class)
  (:require [clojure.pprint :as pp]
            [gaia-project.game :as g]))


(defn return-after-print [r & s]
  (println (apply str s))
  r)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Passing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pass-current-player [{:keys [turn] :as game}]
  (-> game
      (update :passed conj (first turn))
      (assoc :turn (drop 1 turn))))


;; Building ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn valid-placement? [game player type location]
  true)

(defn place-building [game player type location]
  game)

(defn build-player [game player type location]
  (cond
    (= (g/max-building-count type) (get-in player [:buildings-out type]))
    (return-after-print game "All " type "s are already out")

    (not (g/enough-resource player :gold (:gold (g/get-cost player type))))
    (return-after-print game "Not enough gold")

    (not (g/enough-resource player :ore (:ore (g/get-cost player type))))
    (return-after-print game "Not enough ore")

    (not (valid-placement? game player type location))
    (return-after-print game "Invalid placement location")

    :else (place-building game player type location)))

(comment
 (-> (g/new-game [:terrans :alien2])
     ;;ignore setup for now
     g/take-income
     pass-current-player
     pass-current-player
     )

 ;; Idea: should be able to run the game by doing this
 ;(-> (new-game [players])
 ;    (setup-turns {})  ;; This summarizes multiple turns for placing first settlements etc.
 ;    ;; People take turns like below
 ;    (build :mine :e9)           ;; type and coordinates
 ;    (up-science :gaiaforming)   ;; track name to go up on
 ;    (pass :transport-3-coins)   ;; round bonus to take
 ;    )
 ;; After each action above, should add/subtract points, move to the next
 ;; turn/round, print the map (& some game state), print a message saying if it
 ;; needs more info from someone (do you want to take power?) or whose turn it
 ;; is (maybe some options), return game


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
