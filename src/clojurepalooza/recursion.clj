(ns clojurepalooza.recursion)

(defn flatten-harder [c]
  (if (not (sequential? c))
    [c]
    (if (not (seq c))
      nil
      (concat (flatten-harder (first c))
              (flatten-harder (rest c))))))

(flatten-harder [[3] 4 :clojurepalooza
                 [2 [8 [9]] :a]
                 [[[[[[[:the-pit]]]]]]]])
(concat [4] [5 6 7 7])
(flatten-harder [3])
;(seq 3)      ;; error
;(seq [3])    ;; (3)
;(sequential? [3])    ;; (3)
