(ns clojurepalooza.calvinball
  (:gen-class))

(defn score-game [game type]
  (let [get-flags (fn [player]
                    (-> (fn [round] (get-in round [player :flags]))
                        (mapv game)
                        (conj 0)))
        doubling-fn (fn [pair] (if (apply identical? pair)
                                 (* 2 (first pair))
                                 (first pair)))
        calc-score (fn [flags]
                     (->> flags
                          (partition 2 1)
                          (map doubling-fn)
                          (apply +)))]
    {:player-1 (calc-score (get-flags :player-1))
     :player-2 (calc-score (get-flags :player-2))}))

(def welsh-1
  [{:player-1 {:flags 5 :score 5} :player-2 {:flags 3 :score 3}}
   {:player-1 {:flags 1 :score 6} :player-2 {:flags 3 :score 9}}
   {:player-1 {:flags 1 :score 8} :player-2 {:flags 3 :score 15}}
   {:player-1 {:flags 7 :score 15} :player-2 {:flags 3 :score 21}}
   {:player-1 {:flags 2 :score 17} :player-2 {:flags 3 :score 27}}
   {:player-1 {:flags 2 :score 21} :player-2 {:flags 2 :score 29}}
   {:player-1 {:flags 1 :score 22} :player-2 {:flags 0 :score 29}}])

(def welsh-2
  [{:player-1 {:flags 2 :score 2} :player-2 {:flags 6 :score 6}}
   {:player-1 {:flags 2 :score 6} :player-2 {:flags 4 :score 10}}
   {:player-1 {:flags 1 :score 7} :player-2 {:flags 9 :score 19}}
   {:player-1 {:flags 3 :score 10} :player-2 {:flags 0 :score 19}}
   {:player-1 {:flags 3 :score 16} :player-2 {:flags 3 :score 22}}
   {:player-1 {:flags 1 :score 17} :player-2 {:flags 1 :score 23}}
   {:player-1 {:flags 0 :score 17} :player-2 {:flags 0 :score 23}}
   {:player-1 {:flags 0 :score 17} :player-2 {:flags 0 :score 23}}
   {:player-1 {:flags 0 :score 17} :player-2 {:flags 0 :score 23}}
   {:player-1 {:flags 0 :score 17} :player-2 {:flags 0 :score 23}}
   {:player-1 {:flags 0 :score 17} :player-2 {:flags 2 :score 25}}])

(def welsh-3
  [{:player-1 {:flags 2 :score 2} :player-2 {:flags 3 :score 3}}
   {:player-1 {:flags 2 :score 6} :player-2 {:flags 3 :score 9}}
   {:player-1 {:flags 2 :score 10} :player-2 {:flags 5 :score 14}}
   {:player-1 {:flags 2 :score 14} :player-2 {:flags 5 :score 24}}
   {:player-1 {:flags 2 :score 18} :player-2 {:flags 1 :score 25}}
   {:player-1 {:flags 2 :score 22} :player-2 {:flags 1 :score 27}}
   {:player-1 {:flags 2 :score 26} :player-2 {:flags 1 :score 29}}
   {:player-1 {:flags 1 :score 27} :player-2 {:flags 0 :score 29}}])

(def welsh-4
  [{:player-1 {:flags 2 :score 2} :player-2 {:flags 3 :score 3}}
   {:player-1 {:flags 2 :score 6} :player-2 {:flags 3 :score 9}}
   {:player-1 {:flags 2 :score 10} :player-2 {:flags 5 :score 14}}
   {:player-1 {:flags 2 :score 14} :player-2 {:flags 5 :score 24}}
   {:player-1 {:flags 8 :score 22} :player-2 {:flags 2 :score 26}}])

(def scottish-1
  [{:player-1 {:flags 2 :score 2} :player-2 {:flags 3 :score 3}}
   {:player-1 {:flags 6 :score 8} :player-2 {:flags 6 :score 9}}
   {:player-1 {:flags 6 :score 20} :player-2 {:flags 2 :score 11}}
   {:player-1 {:flags 5 :score 25} :player-2 {:flags 5 :score 16}}
   {:player-1 {:flags 6 :score 31} :player-2 {:flags 5 :score 26}}
   {:player-1 {:flags 0 :score 31} :player-2 {:flags 4 :score 30}}])

(def scottish-2
  [{:player-1 {:flags 10 :score 10} :player-2 {:flags 0 :score 0}}
   {:player-1 {:flags 4 :score 14} :player-2 {:flags 4 :score 4}}
   {:player-1 {:flags 5 :score 19} :player-2 {:flags 4 :score 12}}
   {:player-1 {:flags 6 :score 25} :player-2 {:flags 4 :score 20}}
   {:player-1 {:flags 3 :score 28} :player-2 {:flags 4 :score 28}}
   {:player-1 {:flags 2 :score 30} :player-2 {:flags 4 :score 36}}])

(def scottish-3
  [{:player-1 {:flags 0 :score 0} :player-2 {:flags 0 :score 0}}
   {:player-1 {:flags 7 :score 7} :player-2 {:flags 1 :score 1}}
   {:player-1 {:flags 3 :score 10} :player-2 {:flags 3 :score 4}}
   {:player-1 {:flags 0 :score 10} :player-2 {:flags 0 :score 4}}
   {:player-1 {:flags 16 :score 26} :player-2 {:flags 20 :score 24}}])

(def test-games [welsh-1 welsh-2 welsh-3 welsh-4
                 scottish-1 scottish-2 scottish-3])

(defn test-score [game]
  (let [final-round (last game)
        correct-score {:player-1 (get-in final-round [:player-1 :score])
                       :player-2 (get-in final-round [:player-2 :score])}]
    (= correct-score (score-game game :welsh))))

(defn test-it-all []
  (let [pass-list (map test-score test-games)]
    (if (every? true? pass-list)
      (println ":)")
      (println ":("))))

(defn -main []
  (test-it-all))

