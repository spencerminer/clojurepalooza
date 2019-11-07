(ns clojurepalooza.haiku)

(defn disqualify [x]
  (clojure.walk/prewalk
    #(cond
       (symbol? %) (symbol (name %))
       ;(string? %) (symbol %)
       :else %)
    x))

(defn subclause []
  (rand-nth '(((but 1) (it 1) (still 1))
              ((that 1) (madly 2)))))

(defn haiku-partition [h]
  (map
    (fn [[taken line-len]]
      (take line-len (drop-while (taken)
                                 (reductions + (map second h)))))
    [[0 5] [5 7] [12 5]]))


(->> `((this 1) (is 1) (a 1) (pretty 2)
       (lousy 2) (haiku 2) ~@(subclause)
       (qualifies 3) (so 1) (there 1))
     disqualify
     (map second)
     (reductions +)
     (split-with #(>= 5 %))
     ((fn [x] (concat (butlast x)
                      (split-with #(>= 12 %) (last x)))))
     (map count)
     ;haiku-partition
     ;;format-for-output
     )