(ns advent-of-code-2021.day8
  (:require [advent-of-code-2021.util :as util]
            [clojure.set :as set]
            [clojure.string :as string]))

(def unparsed-lines (util/get-lines "day8"))

(defn parse-line [line]
  (let [[input _ output]
        (partition-by #{"|"} (string/split line #" "))]
    {:input input
     :output output}))

(def all-displays (map parse-line unparsed-lines))

(def seg-count->digit
  {2 :one
   3 :seven
   4 :four
   7 :eight})

(defn easy-digit? [segments]
  ((set (keys seg-count->digit))
   (count segments)))

(defn count-easy-output-digits [{:keys [output] :as _display}]
  (count (filter easy-digit? output)))

(def part-1 (->> all-displays
                 (map count-easy-output-digits)
                 (apply +)))
part-1  ;; => 543

(defn find-matching-superset [subset supersets]
  (->> supersets
       (filter (fn [superset]
                 (empty? (set/difference subset superset))))
       first))

(defn generate-easy-digits-map [display]
  (reduce (fn [acc segs]
            (case (count segs)
              2 (assoc acc :k-1 (set segs))
              3 (assoc acc :k-7 (set segs))
              4 (assoc acc :k-4 (set segs))
              7 (assoc acc :k-8 (set segs))
              5 (update acc :p-235 conj (set segs))
              6 (update acc :p-069 conj (set segs))))
          {}
          (:input display)))

(defn generate-segs->digit-map [display]
  (let [{:keys [k-1 k-4 k-7 k-8 p-235 p-069]} (generate-easy-digits-map display)

        ;; Of 0, 6, 9, only 9 completely contains 4
        k-9 (find-matching-superset k-4 p-069)
        p-06 (remove #{k-9} p-069)

        ;; Of 0, 6, only 0 completely contains 1
        k-0 (find-matching-superset k-1 p-06)
        k-6 (first (remove #{k-0} p-06))

        ;; Of 2, 3, 5, only 3 completely contains 1
        k-3 (find-matching-superset k-1 p-235)
        p-25 (remove #{k-3} p-235)

        ;; 2 and 4 overlap in 2 segments, 5 and 4 overlap in 3 segments
        [k-2 k-5] (if (= 2 (count (set/intersection k-4 (first p-25))))
                    p-25
                    ((juxt last first) p-25))]
    {k-0 0
     k-1 1
     k-2 2
     k-3 3
     k-4 4
     k-5 5
     k-6 6
     k-7 7
     k-8 8
     k-9 9}))

(defn solve-display [display]
  (let [segs->digit (generate-segs->digit-map display)]
    (->> display
         :output
         (map (comp segs->digit set))
         (apply str)
         parse-long)))

(def part-2 (apply + (map solve-display all-displays)))
part-2  ;; => 994266
