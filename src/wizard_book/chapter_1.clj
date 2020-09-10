(ns wizard-book.chapter-1
  (:require [java-time :as jt]))

(defn square [x]
  (* x x))

(defn divides? [a b]
  (zero? (mod b a)))

(defn- find-divisor [n test-divisor]
  (cond
    (< n (square test-divisor)) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (inc test-divisor))))

(defn- smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

;; Exercise 1.22
(defn- timed-prime-test [n]
  (printf "%8s : " n)
  (time (prime? n)))

(defn- system-timed-prime-test
  "I like being able to output the time rather than just printing it. But this
  prime calculation happens SOOO fast that it rounds to 0"
  [n]
  (let [start-time (System/currentTimeMillis)]
    (when (prime? n)
      [n (- (System/currentTimeMillis) start-time)])))

(defn- closest-odd [n]
  (if (even? n) (inc n) n))

(defn- search-for-primes [lo hi]
  (filter prime?
          (range (closest-odd lo) (inc hi) 2)))

(comment
 (for [[lo hi] [[1000 10000]
                [10000 100000]
                [100000 1000000]
                [1000000 10000000]]]
   (map timed-prime-test (take 3 (search-for-primes lo hi)))
   #_(map system-timed-prime-test (take 3 (search-for-primes lo hi))))
 "    1009 : Elapsed time: 0.002790 msecs"
 "    1013 : Elapsed time: 0.002075 msecs"
 "    1019 : Elapsed time: 0.002036 msecs"
 "   10007 : Elapsed time: 0.005412 msecs"
 "   10009 : Elapsed time: 0.005378 msecs"
 "   10037 : Elapsed time: 0.005842 msecs"
 "  100003 : Elapsed time: 0.016762 msecs"
 "  100019 : Elapsed time: 0.016594 msecs"
 "  100043 : Elapsed time: 0.018053 msecs"
 " 1000003 : Elapsed time: 0.053358 msecs"
 " 1000033 : Elapsed time: 0.053273 msecs"
 " 1000037 : Elapsed time: 0.052767 msecs"

 "The `prime?` function uses an algorithm that increases in time complexity"
 " by an order √n. This is borne out by the testing that shows that primes that"
 " are 10 times bigger take about √10 times longer to compute.")


;; Exercise 1.23
(defn- next-test-divisor [n]
  (if (= 2 n) 3 (+ 2 n)))

(defn- new-find-divisor [n test-divisor]
  (cond
    (< n (square test-divisor)) n
    (divides? test-divisor n) test-divisor
    :else (new-find-divisor n (next-test-divisor test-divisor))))

(defn- new-smallest-divisor [n]
  (new-find-divisor n 2))

(defn- new-prime? [n]
  (= n (new-smallest-divisor n)))

(defn- new-timed-prime-test [n]
  (printf "%8s : " n)
  (time (new-prime? n)))

(comment
 (for [[lo hi] [[1000 10000]
                [10000 100000]
                [100000 1000000]
                [1000000 10000000]]]
   (map (fn [n]
          (timed-prime-test n)
          (new-timed-prime-test n)
          (println))
        (take 3 (search-for-primes lo hi))))
 "    1009 : Elapsed time: 0.002993 msecs"
 "    1009 : Elapsed time: 0.006273 msecs"

 "    1013 : Elapsed time: 0.002771 msecs"
 "    1013 : Elapsed time: 0.002836 msecs"

 "    1019 : Elapsed time: 0.002270 msecs"
 "    1019 : Elapsed time: 0.002542 msecs"

 "   10007 : Elapsed time: 0.010051 msecs"
 "   10007 : Elapsed time: 0.004020 msecs"

 "   10009 : Elapsed time: 0.005570 msecs"
 "   10009 : Elapsed time: 0.009474 msecs"

 "   10037 : Elapsed time: 0.006597 msecs"
 "   10037 : Elapsed time: 0.004016 msecs"

 "  100003 : Elapsed time: 0.030562 msecs"
 "  100003 : Elapsed time: 0.020460 msecs"

 "  100019 : Elapsed time: 0.038641 msecs"
 "  100019 : Elapsed time: 0.020245 msecs"

 "  100043 : Elapsed time: 0.046396 msecs"
 "  100043 : Elapsed time: 0.020267 msecs"

 " 1000003 : Elapsed time: 0.115572 msecs"
 " 1000003 : Elapsed time: 0.061881 msecs"

 " 1000033 : Elapsed time: 0.155044 msecs"
 " 1000033 : Elapsed time: 0.086288 msecs"

 " 1000037 : Elapsed time: 0.121379 msecs"
 " 1000037 : Elapsed time: 0.073210 msecs"

 "For the largest prime numbers, the new verification method is almost twice as"
 " fast, but for the mid-size primes calculation is maybe 1/3 faster, and for"
 " the smaller primes the new method is actually slower! I guess the logic in"
 " `next-test-divisor` is actually more expensive than the running of"
 " `new-find-divisor` that it saves?")

;; Exercise 1.24
(defn- expmod [base exp m]
  (cond
    (zero? exp) 1
    (even? exp) (mod (square (expmod base (/ exp 2) m))
                     m)
    :else (mod (* base (expmod base (dec exp) m))
               m)))

(defn- fermat-test [n]
  (let [a (inc (rand-int (dec n)))]
    (= (expmod a n n) a)))

(defn fast-prime? [n times]
  (cond
    (zero? times) true
    (fermat-test n) (fast-prime? n (dec times))
    :else false))

(defn- fast-timed-prime-test [n times]
  (printf "%8s : " n)
  (time (fast-prime? n times)))

(comment
 (for [[lo hi] [[1000 10000]
                [10000 100000]
                [100000 1000000]
                [1000000 10000000]]]
   (map (fn [n]
          (timed-prime-test n)
          (new-timed-prime-test n)
          (fast-timed-prime-test n 3)
          (println))
        (take 3 (search-for-primes lo hi))))
 "   1009 : Elapsed time: 0.003001 msecs"
 "   1009 : Elapsed time: 0.005373 msecs"
 "   1009 : Elapsed time: 0.017497 msecs"

 "   1013 : Elapsed time: 0.011287 msecs"
 "   1013 : Elapsed time: 0.003128 msecs"
 "   1013 : Elapsed time: 0.020446 msecs"

 "   1019 : Elapsed time: 0.002837 msecs"
 "   1019 : Elapsed time: 0.002932 msecs"
 "   1019 : Elapsed time: 0.010477 msecs"

 "  10007 : Elapsed time: 0.020063 msecs"
 "  10007 : Elapsed time: 0.004900 msecs"
 "  10007 : Elapsed time: 0.022641 msecs"

 "  10009 : Elapsed time: 0.007796 msecs"
 "  10009 : Elapsed time: 0.005737 msecs"
 "  10009 : Elapsed time: 0.022152 msecs"

 "  10037 : Elapsed time: 0.006455 msecs"
 "  10037 : Elapsed time: 0.004896 msecs"
 "  10037 : Elapsed time: 0.016736 msecs"

 " 100003 : Elapsed time: 0.040445 msecs"
 " 100003 : Elapsed time: 0.048981 msecs"
 " 100003 : Elapsed time: 0.017675 msecs"

 " 100019 : Elapsed time: 0.036202 msecs"
 " 100019 : Elapsed time: 0.033965 msecs"
 " 100019 : Elapsed time: 0.024595 msecs"

 " 100043 : Elapsed time: 0.036890 msecs"
 " 100043 : Elapsed time: 0.041121 msecs"
 " 100043 : Elapsed time: 0.027894 msecs"

 "1000003 : Elapsed time: 0.165397 msecs"
 "1000003 : Elapsed time: 0.087248 msecs"
 "1000003 : Elapsed time: 0.067346 msecs"

 "1000033 : Elapsed time: 0.156836 msecs"
 "1000033 : Elapsed time: 0.077084 msecs"
 "1000033 : Elapsed time: 0.051996 msecs"

 "1000037 : Elapsed time: 0.161939 msecs"
 "1000037 : Elapsed time: 0.108274 msecs"
 "1000037 : Elapsed time: 0.064966 msecs"

 "The `fast-prime?` function uses the Fermat test which has O(log(n)) growth"
 " and the runtime for larger primes seems to increase roughly logarithmically")


;; Exercise 1.30
(defn cube [x] (* x x x))

(defn- sum-rec [term a nxt b]
  (if (> a b)
    0
    (+ (term a)
       (sum-rec term (nxt a) nxt b))))

(defn- sum-cubes-rec [a b]
  (sum-rec cube a inc b))

(defn- sum-iter [term a nxt b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (iter (nxt a)
                    (+ (term a) result))))]
    (iter a 0)))

(defn- sum-cubes-iter [a b]
  (sum-iter cube a inc b))

(comment
 (sum-cubes-iter 1 10)
 (sum-cubes-rec 1 10))


;; Exercise 1.33
(defn- filtered-accumulate-rec [pred combiner null-value term a nxt b]
  (if (> a b)
    null-value
    (combiner
     (if (pred a) (term a) null-value)
     (filtered-accumulate-rec pred combiner null-value term (nxt a) nxt b))))

(defn- sum-cubes-crazy-rec [a b]
  (filtered-accumulate-rec identity + 0 cube a inc b))

(defn- sum-squared-primes-rec [a b]
  (filtered-accumulate-rec prime? + 0 square a inc b))

(defn- filtered-accumulate-iter [pred combiner null-value term a nxt b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (iter (nxt a)
                    (combiner (if (pred a) (term a) null-value) result))))]
    (iter a null-value)))

(defn- sum-cubes-crazy-iter [a b]
  (filtered-accumulate-iter identity + 0 cube a inc b))

(defn- sum-squared-primes-iter [a b]
  (filtered-accumulate-iter prime? + 0 square a inc b))

(comment
 (sum-cubes-crazy-rec 1 10)
 (sum-cubes-crazy-iter 1 10)
 (sum-squared-primes-rec 1 10)
 (sum-squared-primes-iter 1 10))


;; Exercise 1.34
(defn- f [g]
  (g 2))

(comment
 (f square)
 (f (fn [z] (* z (inc z))))

 "The below will fail, of course"
 (f f)
 (f 2)
 (2 2)

 "`let` in Clojure is a lot more complicated than the simple `let` defined in"
 " SICP Scheme.")


(defn decimal-places-of-accuracy [a b]
  (-> a
      (- b)
      Math/abs
      Math/log
      (/ (Math/log 10))
      Math/ceil
      -))

;; Exercise 1.37
(def const-golden-ratio 1.61803398874989484820)

(defn- cont-frac-rec
  ([numer-fn denom-fn k]
   (cont-frac-rec numer-fn denom-fn k 1))
  ([numer-fn denom-fn k i]
   (if (= i k)
     (/ (numer-fn i) (denom-fn i))
     (/ (numer-fn i) (+ (denom-fn i)
                        (cont-frac-rec numer-fn denom-fn k (inc i)))))))

(defn- cont-frac-iter [numer-fn denom-fn k]
  (reduce (fn [acc i]
            (/ (numer-fn i)
               (+ (denom-fn i) acc)))
          (range k 0 -1)))

(comment
 (/ 1 (cont-frac-rec (constantly 1.0) (constantly 1.0) 30))
 (/ 1 (cont-frac-iter (constantly 1.0) (constantly 1.0) 30))
 "These are both accurate up to the same number of decimal places, but the"
 " digits after that are different!"

 (decimal-places-of-accuracy
   const-golden-ratio
   (/ 1 (cont-frac-iter (constantly 1.0) (constantly 1.0) 39)))
 (decimal-places-of-accuracy
   const-golden-ratio
   (/ 1 (cont-frac-iter (constantly 1.0) (constantly 1.0) 39)))
 )

;; Exercise 1.38
(def const-e 2.71828182845904523536)

(defn- e-cont-frac-denom-fn [i]
  (if (= 2 (mod i 3))
    (-> i inc (* 2/3))
    1))

(comment
 (+ 2 (cont-frac-iter (constantly 1.0) e-cont-frac-denom-fn 20))
 (+ 2 (cont-frac-rec (constantly 1.0) e-cont-frac-denom-fn 20))
 (decimal-places-of-accuracy
   const-e
   (+ 2.0 (cont-frac-iter (constantly 1.0) e-cont-frac-denom-fn 21)))
 )


;; Exercise 1.41
(defn- double-single-arity-func [f]
  (fn [x]
    (f (f x))))

(comment
 (((double-single-arity-func
    (double-single-arity-func double-single-arity-func))
   inc) 5)
 ;; => 21
 )


;; Exercise 1.
(defn- repeated-they-call-it [f n]
  (fn [x]
    (nth (iterate f x) n)))

(defn- repeated-they-call-it-using-comp [f n]
  (if (= 1 n)
    f
    (comp f (repeated-they-call-it-using-comp f (dec n)))))

(comment
 ((repeated-they-call-it square 4) 5)
 ((repeated-they-call-it-using-comp square 4) 5)
 )
