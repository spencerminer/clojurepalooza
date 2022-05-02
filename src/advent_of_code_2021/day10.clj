(ns advent-of-code-2021.day10
  (:require [advent-of-code-2021.util :as util]))

(def lines (util/get-lines "day10"))

(def opener? #{\( \[ \{ \<})
(def opener->closer {\( \)
                     \[ \]
                     \{ \}
                     \< \>})

(defn match-line [line]
  (reduce (fn [stack next-delimiter]
            (cond
              (opener? next-delimiter)
              (conj stack next-delimiter)

              (= (opener->closer (last stack))
                 next-delimiter)
              (vec (drop-last stack))

              :else (reduced (str (last stack) next-delimiter))))
          []
          line))

(def score-illegal-char {\) 3
                         \] 57
                         \} 1197
                         \> 25137})

(def part-1 (->> lines
                 (map match-line)
                 (filter string?)
                 (map second)
                 (map score-illegal-char)
                 (apply +)))
part-1  ;; => 345441


(def score-closer-char {\) 1
                        \] 2
                        \} 3
                        \> 4})

(defn score-closers [closers]
  (reduce (fn [score next-closer]
            (->> score
                 (* 5)
                 (+ (score-closer-char next-closer))))
          0
          closers))

(defn median [coll]
  (let [n (count coll)]
    (if (odd? n)
      (nth (sort coll)
           (/ (dec n) 2))
      (ex-info "Even count list unimplemented" {}))))

(def part-2 (->> lines
                 (map match-line)
                 (filter coll?)
                 (map (fn [unmatched-chars]
                        (map opener->closer unmatched-chars)))
                 (map reverse)
                 (map score-closers)
                 median))
part-2 ;; => 3235371166
