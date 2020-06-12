(ns clojurepalooza.time-stuff
  (:require [clj-time.core :as t]))

(defn time-range [from to]
  (take-while
    (fn [t] (t/before? t to))
    (iterate (fn [t] (t/plus t (t/days 1))) from)))

(time-range (t/local-date 2020 1 22) (t/today))

(t/today)

(defn tseq [t]
  (lazy-seq (cons t (tseq (t/plus t (t/days 1))))))
