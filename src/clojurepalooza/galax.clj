(ns clojurepalooza.galax)

(defn disqualify [x]
  (clojure.walk/prewalk
    #(cond
       (symbol? %) (symbol (name %))
       (string? %) (symbol %)
       :else %)
    x))

(defmacro defnth [name l]
  `(defn ~name []
     (rand-nth ~l)))

(defnth color `(grey white yellow orange red purple blue black green))

(defnth color-pair `(greyish-white
                      greyish-yellow
                      yellow-orange
                      orange-red
                      purplish-blue
                      greenish-black
                      blue-green
                      grey-green
                      purple-grey
                      reddish-black))
(defnth lifeforms `(algae nanobes viruses
                          "carbon chains"
                          "complex molecules"))
(defnth adjective `(~(color) ~(color-pair) slimy oozing gaseous vitreous microscopic metallic))
(defnth evolved `("evolved" "started reproducing" "begun self-replicating"))
(defnth place-adj `(dark deep smoky lava-filled wet icy sunny scorched))

(defnth place-noun `(canyons pits valleys mountaintops))

(defmacro either [a b]
  `(if (zero? (rand-int 2)) ~a ~b))

(defn itsalive [planet]
  (disqualify
    (either
      `(~(adjective) ~(lifeforms) have ~(evolved)
         in the ~(place-adj) ~(place-noun) of planet ~(str planet "!"))
      `(in the ~(place-adj) ~(place-noun) of planet ~(str planet "~")
           ~(adjective) ~(lifeforms) have ~(str (evolved) "!")))))

(itsalive "Zorks")