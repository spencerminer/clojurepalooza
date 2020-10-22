(ns clojurepalooza.ultra-lazy)

;; Clojurepalooza 2020-09-25

;; Take a look at this algorithmic exercise from Gary Fredericks:
;;
;; https://twitter.com/gfredericks_/status/1308176156198350848
;;
;;     Writing a function that lazily merges an ordered lazy seq of ordered lazy
;;     seqs in #clojure was a lot thornier than I expected.
;;
;;     https://gist.github.com/gfredericks/f75c95eb06fc4deacf6cf3aa9cdabb27
;;
;;     I'm curious if anybody can restructure it to make it simpler and maybe
;;     easier to understand.
;;
;; Apparently it's really hard.

;; Ted came up with this example input:
(->> (map (fn [i] (map (partial * i) (rest (range))))
          (rest (range)))
     (take 7)
     (map (partial take 7)))
;; =>
'((1  2  3  4  5  6  7 #_...)
  (2  4  6  8 10 12 14 #_...)
  (3  6  9 12 15 18 21 #_...)
  (4  8 12 16 20 24 28 #_...)
  (5 10 15 20 25 30 35 #_...)
  (6 12 18 24 30 36 42 #_...)
  (7 14 21 28 35 42 49 #_...)
  #_...
  )

;; The correct output for this input starts like this:

'(1 2 2 3 3 4 4 4 5 5 6 6 6 6 7 7 #_...)

;; Let's consider simplified versions of the problem and use what we learn to
;; gain insight on the considerations necessary for a full solution.

;; ===================================

(defn my-merge-2
  "Merge two ordered sequences"
  [[l1 l2]]
  (sort (concat l1 l2)))

(assert (= (my-merge-2 '((1  2  3  4  5  6  7)
                         (2  4  6  8 10 12 14)))
           '(1 2 2 3 4 4 5 6 6 7 8 10 12 14))
        "my-merge-2 does not yet work")

;; HINT: use `compare` instead of `<`
(assert (= (my-merge-2 '(("a" "b" "d" "g")
                         ("a" "e" "f" "g")))
           '("a" "a" "b" "d" "e" "f" "g" "g"))
        "my-merge-2 does not yet work on general Comparable objects")

;; ===================================

(defn my-merge-3
  "Merge three ordered sequences"
  [list-of-lists]
  (sort (apply concat list-of-lists)))
(assert (= (my-merge-3 '((1  2  3  4  5  6  7)
                         (2  4  6  8 10 12 14)
                         (3  6  9 12 15 18 21)))
           '(1 2 2 3 3 4 4 5 6 6 6 7 8 9 10 12 12 14 15 18 21))
        "my-merge-3 does not yet work.")

;; ===================================

(defn my-merge-2-lazy
  "Lazily merge two lazy ordered sequences"
  [[l1 l2]]
  (lazy-seq
   (if (< (first l1) (first l2))
     (cons (first l1) (my-merge-2-lazy [(rest l1) l2]))
     (cons (first l2) (my-merge-2-lazy [l1 (rest l2)]))))

  #_(conj [] (apply min (map first [l1 l2])))

  #_(let [a (map first list-of-lists)
        b (map second list-of-lists)]
    (take-while #(< % (first b)) a))
  #_(sort (apply concat list-of-lists)))

(let [input (list (range)
                  (map (partial * 2) (range)))
      expected (->> input
                    (map (partial take 20))
                    (reduce concat [])
                    sort
                    (take 20))]
  #_(take 20 (my-merge-2-lazy input))
  (assert (= (take 20 (my-merge-2-lazy input))
             expected)
          "my-merge-2-lazy does not yet work."))

;; If you think it will be a useful exercise, try extending this to three lazy
;; ordered sequences.

;; ===================================

(defn my-merge-n-lazy
  "Lazily merge 2+ lazy ordered sequences"
  [list-of-lists]
  (if (= 2 (count list-of-lists))
    (my-merge-2-lazy list-of-lists)
    (my-merge-n-lazy (list (my-merge-2-lazy (take 2 list-of-lists))
                           (drop 2 list-of-lists))))
  #_(lazy-seq
   (if (< (first l1) (first l2))
     (cons (first l1) (my-merge-n-lazy [(rest l1) l2]))
     (cons (first l2) (my-merge-n-lazy [l1 (rest l2)])))))

(let [input (list (range)
                  (map (partial * 2) (range))
                  (map (partial * 3) (range)))
      expected (->> input
                    (map (partial take 20))
                    (reduce concat [])
                    sort
                    (take 20))]
  (take 20 (my-merge-n-lazy input))
  #_(assert (= (take 20 (my-merge-n-lazy input))
             expected)
          "my-merge-2-lazy does not yet work."))

;; If you think it will be a useful exercise, try extending this to four lazy
;; ordered sequences.

;; ===================================

(defn my-merge-lazy-pairs
  "Lazily merge an ordered lazy sequence of pairs"
  [_]
  ())
(let [input (map (juxt identity (partial * 2)) (range))
      expected (->> input
                    (take 20)
                    (reduce concat [])
                    sort
                    (take 20))]
  (assert (= (take 20 (my-merge-lazy-pairs input))
             expected)
          "my-merge-lazy-pairs does not yet work."))

;; If you think it will be a useful exercise, try extending this to triplets
;; instead of pairs.

;; ===================================

;; I don't think we'll get this far in an hour but if we do, we can try the
;; original problem!