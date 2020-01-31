(ns clojurepalooza.continuation-passing-style)

(defn *& [x y k]
  (k (* x y)))

(defn -& [x y k]
  (k (- x y)))

(defn =& [x y k]
  (k (= x y)))

(defn factorial& [n k]
  (=& n 0 (fn [b]
            (if b
              (k 1)
              (-& n 1 (fn [nm1]
                        (factorial& nm1 (fn [f]
                                          (*& n f k)))))))))

(defn divide& [x y k err]
  (=& y 0 (fn [denominator-is-zero]
            (if denominator-is-zero
              (err (str "Divide by zero! " x "/0"))
              (k (/ x y))))))

(divide& 3 0 identity println)
