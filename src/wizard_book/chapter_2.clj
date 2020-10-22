(ns wizard-book.chapter-2
  (:require [wizard-book.chapter-1 :as w1]))

;; Exercise 2.5
(defn exp [base exponent]
  (apply *' (repeat exponent base)))

(defn- cons23 [a b]
  (*' (exp 2 a) (exp 3 b)))

(defn- car-cdr-helper [base z]
  (->> z
       (iterate #(/ % base))
       (take-while integer?)
       count
       dec))

(defn- car23 [z]
  (car-cdr-helper 2 z))

(defn- cdr23 [z]
  (car-cdr-helper 3 z))

(comment
 (car23 (cons23 2 30))
 (cdr23 (cons23 2 30))
 (car23 (cons23 13 6))
 (cdr23 (cons23 13 6))
 (car23 (cons23 15 45))
 (cdr23 (cons23 15 45))

 )

;; Extended Exercise 2.1.4 ;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.7
(defn- make-interval [a b] [a b])
(defn- lower-bound [z] (first z))
(defn- upper-bound [z] (second z))

;; Exercise 2.8
(defn- sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(comment
 (sub-interval (make-interval 9 11)
               (make-interval 3 7))
 (sub-interval (make-interval 3 7)
               (make-interval 9 11))
 (sub-interval (make-interval -4 -1)
               (make-interval 9 11)))

;; Exercise 2.10
(defn- mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    #_[p1 p2 p3 p4]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn- div-interval [x y]
  (if (< (lower-bound y) 0 (upper-bound y))
    (println "Dividing by an interval that spans 0 is weird")
    (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(comment
 (div-interval (make-interval 9 11)
               (make-interval 3 7))
 (div-interval (make-interval 9 11)
               (make-interval -3 7)))

;; Exercise 2.11
(comment
 "First is 2,4"
 (mul-interval (make-interval 2 4)                          ;; 1 4
               (make-interval 2 4))
 (mul-interval (make-interval 2 4)                          ;; 3 4
               (make-interval -2 4))
 (mul-interval (make-interval 2 4)                          ;; 3 2
               (make-interval -4 -2))

 "First is -2,4"
 (mul-interval (make-interval -2 4)                         ;; 2 4
               (make-interval 2 4))
 (mul-interval (make-interval -2 4)                         ;; 2/3 4
               (make-interval -2 4))
 (mul-interval (make-interval -2 3)                         ;; 2/3 4
               (make-interval -2 4))
 (mul-interval (make-interval -2 4)                         ;; 2/3 4
               (make-interval -2 3))
 (mul-interval (make-interval -2 4)                         ;; 3 1
               (make-interval -4 -2))


 "First is -4,-2"
 (mul-interval (make-interval -4 -2)                        ;; 2 3
               (make-interval 2 4))
 (mul-interval (make-interval -4 -2)                        ;; 2 1
               (make-interval -2 4))
 (mul-interval (make-interval -4 -2)                        ;; 4 1
               (make-interval -4 -2))
 )

(defn- bens-mul-interval
  "This uses the 9-possibility logic that Ben mentioned, but doesn't actually
  use that to reduce the number of operations carried out. This is to make it
  easier to understand."
  [x y]
  (let [lo-x (lower-bound x)
        hi-x (upper-bound x)
        lo-y (lower-bound y)
        hi-y (upper-bound y)
        p1 (* lo-x lo-y)
        p2 (* lo-x hi-y)
        p3 (* hi-x lo-y)
        p4 (* hi-x hi-y)]
    (cond
      (and (pos? lo-x) (pos? hi-x))
      (cond
        (and (pos? lo-y) (pos? hi-y))
        (make-interval p1 p4)

        (and (neg? lo-y) (pos? hi-y))
        (make-interval p3 p4)

        (and (neg? lo-y) (neg? hi-y))
        (make-interval p3 p2))

      (and (neg? lo-x) (pos? hi-x))
      (cond
        (and (pos? lo-y) (pos? hi-y))
        (make-interval p2 p4)

        (and (neg? lo-y) (pos? hi-y))
        (make-interval (min p2 p3) p4)

        (and (neg? lo-y) (neg? hi-y))
        (make-interval p3 p1))

      (and (neg? lo-x) (neg? hi-x))
      (cond
        (and (pos? lo-y) (pos? hi-y))
        (make-interval p2 p3)

        (and (neg? lo-y) (pos? hi-y))
        (make-interval p2 p1)

        (and (neg? lo-y) (neg? hi-y))
        (make-interval p4 p1)))))

(comment
 (for [x [(make-interval 2 5)
          (make-interval -2 5)
          (make-interval -5 -2)]
       y [(make-interval 3 4)
          (make-interval -3 4)
          (make-interval -4 -3)]]
   [[x y] (= (mul-interval x y) (bens-mul-interval x y))])

 (= (mul-interval (make-interval 2 4) (make-interval 2 4))
    (bens-mul-interval (make-interval 2 4) (make-interval 2 4)))
 (= (mul-interval (make-interval 2 4) (make-interval -2 4))
    (bens-mul-interval (make-interval 2 4) (make-interval -2 4)))
 (= (mul-interval (make-interval 2 4) (make-interval -4 -2))
    (bens-mul-interval (make-interval 2 4) (make-interval -4 -2)))
 (= (mul-interval (make-interval -2 4) (make-interval 2 4))
    (bens-mul-interval (make-interval -2 4) (make-interval 2 4)))
 (= (mul-interval (make-interval -2 4) (make-interval -2 4))
    (bens-mul-interval (make-interval -2 4) (make-interval -2 4)))
 (= (mul-interval (make-interval -2 3) (make-interval -2 4))
    (bens-mul-interval (make-interval -2 3) (make-interval -2 4)))
 (= (mul-interval (make-interval -2 4) (make-interval -2 3))
    (bens-mul-interval (make-interval -2 4) (make-interval -2 3)))
 (= (mul-interval (make-interval -2 4) (make-interval -4 -2))
    (bens-mul-interval (make-interval -2 4) (make-interval -4 -2)))
 (= (mul-interval (make-interval -4 -2) (make-interval 2 4))
    (bens-mul-interval (make-interval -4 -2) (make-interval 2 4)))
 (= (mul-interval (make-interval -4 -2) (make-interval -2 4))
    (bens-mul-interval (make-interval -4 -2) (make-interval -2 4)))
 (= (mul-interval (make-interval -4 -2) (make-interval -4 -2))
    (bens-mul-interval (make-interval -4 -2) (make-interval -4 -2)))
 )

;; Exercise 2.17
(defn- cons-scheme [a b] [a b])
(defn- car-scheme [x] (first x))
(defn- cdr-scheme [x] (drop 1 x))

(defn- last-pair [l]
  (if (empty? (cdr-scheme l))
    (car-scheme l)
    (last-pair (cdr-scheme l))))

(comment
 (last-pair (list 12 23 34 45 56 67)))


;; Exercise 2.18
(defn- reverse-schemey [l]
  (let [rev-iter
        (fn [re fw]
          (if (empty? fw)
            re
            (recur (cons (car-scheme fw) re)
                   (cdr-scheme fw))))]
    (rev-iter [] l)))

(comment
 (reverse-schemey (list 1 2 3 4 5 9 10 14)))


;; Exercise 2.20
(defn- same-parity [x & xs]
  (->> xs
       (filter (if (even? x) even? odd?))                   ;;#(= (rem x 2) (rem % 2))
       (cons x)))

(defn- same-parity-iter [x & xs]
  (let [correct-parity? (if (even? x) even? odd?)
        iter (fn iter [acc l]
               (if (empty? l)
                 acc
                 (if (correct-parity? (car-scheme l))
                   (iter (cons (car-scheme l) acc) (cdr-scheme l))
                   (iter acc (cdr-scheme l)))))]
    (cons x (iter [] xs))))

(comment
 (same-parity 4 3 5 6 7 8 9 0 10 11 12 13)
 (same-parity 9 4 3 5 6 7 8 9 0 10 11 12 13)
 (same-parity-iter 4 3 5 6 7 8 9 0 10 11 12 13)
 (same-parity-iter 9 4 3 5 6 7 8 9 0 10 11 12 13))


;; Exercise 2.22
(comment
 "If you cons the squared item of the list with the accumulator `answer`, then"
 " the first item from the input list will be last in the output list. If you"
 " cons them in the opposite order, then you get a linked list that starts with"
 " a nil, which is invalid.")

;; Exercise 2.23
(defn for-each
  "It's basically a map version of doseq"
  [proc items]
  (if (empty? items)
    "You're done!"
    (do (proc (car-scheme items))
        (for-each proc (cdr-scheme items)))))

(comment
 (for-each (fn [x] (println x) x)
           (list 57 321 88)))


;; Exercise 2.27
(defn- deep-reverse-recurs [l]
  (when (seq l)
    (cons (if (coll? (last l))
            (deep-reverse-recurs (last l))
            (last l))
          (deep-reverse-recurs (butlast l)))))

(defn- deep-reverse-iter
  "It's iterative when it's reversing shallowly, but it calls itself to reverse
   the inner structures"
  [l]
  (let [deep-reverse-semi-iter
        (fn [in out]
          (if (empty? in)
            out
            (recur (rest in)
                   (cons (if (coll? (first in))
                           (deep-reverse-iter (first in))
                           (first in))
                         out))))]
    (deep-reverse-semi-iter l [])))

(comment
 (cons 1 (cons 3 (list 4)))
 (deep-reverse-recurs (list 1 2))
 (deep-reverse-recurs (list (list 1 2) (list 3 4)))
 (deep-reverse-iter (list 1 2))
 (deep-reverse-iter (list (list 1 2) (list 3 4)))
 )


;; Exercise 2.28
(defn- fringe
  "Flatten"
  [tree-list]
  #_(flatten tree-list)
  #_(cond
      (empty? tree-list) nil

      (coll? (first tree-list))
      (concat (fringe (first tree-list))
              (fringe (rest tree-list)))
      :else
      (cons (first tree-list)
            (fringe (rest tree-list))))

  (let [m (if (coll? (first tree-list))
            {:merge-method concat, :first-method fringe}
            {:merge-method cons, :first-method identity})]
    (when (seq tree-list)
      ((:merge-method m) ((:first-method m) (first tree-list))
       (fringe (rest tree-list))))))

(comment
 (concat (list 1 2) (list 3 4))
 (def x (list (list 1 2) (list 3 4)))
 (fringe x)
 (fringe (list x x))
 )


;; Exercise 2.29
(defn- make-mobile [left right] (list left right))
(defn- make-branch [length structure] (list length structure))
(defn- left-branch [mobile] (first mobile))
(defn- right-branch [mobile] (second mobile))
(defn- branch-length [branch] (first branch))
(defn- branch-structure [branch] (second branch))

(defn- total-weight-old [mobile]
  (let [l-b (left-branch mobile)
        r-b (right-branch mobile)
        branch-weight (fn [branch]
                        (let [struct (branch-structure branch)]
                          (if (coll? struct)
                            (total-weight-old struct)
                            struct)))]
    (+ (branch-weight l-b)
       (branch-weight r-b))))

(defn- total-weight [mobile]
  (let [branch-weight (fn [branch]
                        (let [struct (branch-structure branch)]
                          (cond-> struct
                            (coll? struct) total-weight)))]
    (if (number? mobile)
      mobile
      (->> mobile
           ((juxt left-branch right-branch))
           (map branch-weight)
           (apply +)))))

(defn- mobile-balanced? [mobile]
  (let [get-torque (fn [branch]
                     (* (branch-length branch)
                        (total-weight (branch-structure branch))))]
    (or (number? mobile)
        (and (= (get-torque (left-branch mobile))
                (get-torque (right-branch mobile)))
             (mobile-balanced? (branch-structure (left-branch mobile)))
             (mobile-balanced? (branch-structure (right-branch mobile)))
             ))))

(defn- mobile-balanced?-ready-for-multi-branch [mobile]
  (let [get-torque (fn [branch]
                     (* (branch-length branch)
                        (total-weight (branch-structure branch))))
        get-branches (juxt left-branch right-branch)]
    (or (number? mobile)
        (and (->> mobile
                  get-branches
                  (map get-torque)
                  (apply =))
             (->> mobile
                  get-branches
                  (map (comp mobile-balanced? branch-structure))
                  (every? true?))))))

(def mobile-1 (make-mobile (make-branch 4 2)
                           (make-branch 3 6)))
(def mobile-2 (make-mobile (make-branch 4 3)
                           (make-branch 6 2)))
(def mobile-3 (make-mobile (make-branch 5 mobile-1)
                           (make-branch 4 mobile-2)))
(def mobile-4 (make-mobile (make-branch 2 mobile-3)
                           (make-branch 7 mobile-2)))
(def mobile-5 (make-mobile (make-branch 3 mobile-2)
                           (make-branch 3 mobile-2)))
(def mobile-6 (make-mobile (make-branch 4 1)
                           (make-branch 1 4)))
(def mobile-7 (make-mobile (make-branch 3 mobile-2)
                           (make-branch 3 mobile-6)))
(def mobile-list
  [mobile-1 mobile-2 mobile-3 mobile-4 mobile-5 mobile-6 mobile-7])

(comment
 (map total-weight-old mobile-list)
 (map total-weight mobile-list)
 (map mobile-balanced? mobile-list)
 (map mobile-balanced?-ready-for-multi-branch mobile-list)

 "The great thing about abstracting out all of the different layers of
 functionality is that if we want to change the implementation of the mobile,
 then we only have to change the constructor and getter functions and
 everything else still just works!"
 )


;; Exercise 2.30
(defn- square-tree-lower-order-functions [tree]
  (cond
    (not (coll? tree)) (w1/square tree)
    (empty? tree) nil
    :else (cons (square-tree-lower-order-functions (first tree))
                (square-tree-lower-order-functions (rest tree)))))

(defn- square-tree-using-maps [tree]
  (map (fn [b]
         (if (coll? b)
           (square-tree-using-maps b)
           (w1/square b)))
       tree))

(def test-tree-1 (list 1
                       (list 2 (list 3 4) 5)
                       (list 6 7)))

(comment
  (square-tree-lower-order-functions test-tree-1)
  (square-tree-using-maps test-tree-1))

;; Exercise 2.31
(defn- tree-map [f tree]
  (map (fn [b]
         (if (coll? b)
           (tree-map f b)
           (f b)))
       tree))

(comment
 (tree-map w1/square test-tree-1)
 (tree-map (comp w1/square inc) test-tree-1))


;; Exericise 2.32
(defn- subsets [s]
  ;(print s "=>")
  (if (empty? s)
    (list nil)
    (let [rest-s (subsets (rest s))]
      (println "rest-s" rest-s)
      (concat rest-s (map (fn [x]
                            (cons (first s) x))
                          rest-s)))))

(comment
 (subsets (list 1 2 3)
          )

 "Now, why does this work?..."
 )
