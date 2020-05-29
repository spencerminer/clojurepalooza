(ns clojurepalooza.game-of-life)

;; 4clojure problem 94 http://www.4clojure.com/problem/94

(defn next-gen [board]
  (let [live? #(= \# %)
        live-neighbors
        (fn [i j]
          (count
            (filter live?
                    (for [x [-1 0 1]
                          y [-1 0 1]
                          :when (not (and (zero? x) (zero? y)))]
                      (get-in board [(+ i x) (+ j y)])))))
        rows (count board)
        cols (count (first board))]
    (vec
      (for [i (range rows)]
        (apply str
               (for [j (range cols)]
                 (let [good-neighbor-count?
                       (if (live? (get-in board [i j]))
                         #{2 3} #{3})]
                   (if (good-neighbor-count? (live-neighbors i j))
                     \#
                     \space))))))))

(def block ["    "
            " ## "
            " ## "
            "    "])

(def blinker ["     "
              "     "
              " ### "
              "     "
              "     "])

(def beacon ["      "
             " ##   "
             " ##   "
             "   ## "
             "   ## "
             "      "])

(def other-beacon ["      "
                   "      "
                   "  ### "
                   " ###  "
                   "      "
                   "      "])

(def glider ["         "
             "  #      "
             "   #     "
             " ###     "
             "         "
             "         "
             "         "
             "         "
             "         "])

(def lwss   ; light-weight spaceship
  ["              "
   " #  #         "
   "     #        "
   " #   #        "
   "  ####        "
   "              "
   "              "
   "              "])

(def pulsar
  ["                 "
   "                 "
   "    ###   ###    "
   "                 "
   "  #    # #    #  "
   "  #    # #    #  "
   "  #    # #    #  "
   "    ###   ###    "
   "                 "
   "    ###   ###    "
   "  #    # #    #  "
   "  #    # #    #  "
   "  #    # #    #  "
   "                 "
   "    ###   ###    "
   "                 "
   "                 "])

(comment

  (doseq [game-round (take 20 (iterate next-gen pulsar))]
    (dorun
      (map #(prn %)
           game-round))
    (Thread/sleep 500))

  )