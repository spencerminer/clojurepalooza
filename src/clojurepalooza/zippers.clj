(ns clojurepalooza.zippers)

(defn deep-random-tree [width depth]
      (if (<= depth 0)
        (rand)
        (-> (range width)
            (->> (map (comp keyword str)))
            (zipmap (repeat (deep-random-tree width (dec depth)))))))

(defn random-path-through-tree [width depth]
      (map (comp keyword str)
           (repeatedly depth #(rand-int width))))

(let [width 10
      depth 10
      tree (deep-random-tree width depth)]
  (time
   (doseq [try-num (range 200)]
     (get-in tree (random-path-through-tree width depth)))))
