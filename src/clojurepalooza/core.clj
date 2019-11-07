(ns clojurepalooza.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn repseq [coll n]
  (mapcat
    #(take n (iterate identity %))
    coll))

(repseq [1 2 3] 3)

(defn interpo [x coll]
  (rest (interleave (repeat x) coll)))

(interpo 1 [3 4 5 6 7])

(defn dropn [coll n]
  (mapcat (partial take (dec n))
          (partition-all n coll)))

(dropn (range 10) 3)

(map (partial +) (range 5))

(defn rev-interleave [coll x]
  (map second (group-by #(mod % x)
                        coll)))

(rev-interleave (range 10) 5)

(defn rot-rec-helper [n coll]
  (if (= n 0)
    coll
    (rot-rec-helper (dec n) (concat (rest coll) [(first coll)]))))

(defn rot-rec [n coll]
  (rot-rec-helper (mod n (count coll)) coll))

(rot-rec 2 [1 2 3 4 5])

(defn rot-func [n coll]
  (let [nmod (mod n (count coll))]
    (concat (drop nmod coll) (take nmod coll))))

(rot-func 2 [1 2 3 4 5])

(defn half-truth [& body]
  (= 2 (count (set body))))

(defn gcd [a b]
  (if (zero? (mod a b))
    b
    (gcd b (mod a b))))

(gcd 10 5)
(gcd 1023 858)
(gcd 5 7)

(defn x-to-the-n [n]
  #(reduce * (repeat n %)))

;; # 166
(fn [op a b]
  (cond
    (op a b) :lt
    (op b a) :gt
    :else :eq))

(fn [a b]
  (apply +
         (map #(* %1 %2) a b)))

(defn cartesian-prod [a b]
  (set (for [x a
             y b]
         [x y])))

(defn andrews-age [phd-years bach-age bizops-time wasting-time]
  (+ phd-years bach-age bizops-time wasting-time))

(andrews-age 6 22 1 1)


(defn lala [x]
  (let [n (count (str x))
        half-x (Integer. (apply str (take (/ n 2) (str x))))]
    half-x))

(lala 100)

([:a :b :c] 1)

(take 10 (interpose :a [2 3 4]))
(take 10 (repeat "x"))


(defn compress [coll]
  (reduce (fn [acc x]
            (if (= x (last acc))
              acc
              (conj acc x)))
          [] coll))
(defn compress2 [coll]
  (->> coll
       (partition-by identity)
       (map first)))
(compress "Leeeeeerrrroyyyy")
(compress2 "Leeeeeerrrroyyyy")

((fn [start end] (take (- end start) (iterate inc start))) 1 4)

((fn [x] (apply * (range 1 (+ x 1)))) 8)
((fn [x] (apply * (take-while pos? (iterate dec x)))) 8)

(for [[x y] (partition 2 (range 20))]
  (+ x y))

(defn dft [d coll] (apply hash-map (interleave coll (repeat d))))
(defn dft2 [d coll] (zipmap coll (repeat d)))
(dft2 0 [:a :b :c])
(dft 0 [:a :b :c])

((fn [s] (apply str (filter #(Character/isUpperCase %) s)))
 "HeLlO, WoRlD!")

(defn flatttn [coll]
  (reduce (fn [acc x]
            (if (coll? x)
              (concat acc (flatttn x))
              (conj (vec acc) x)))
          []
          coll))
(flatttn '((1 2) 3 [4 [5 6]]))
(flatttn ["a" ["b"] "c"])
(flatttn '((((:a)))))

((fn [s c] [(take s c) (drop s c)])
 3 [1 2 3 4 5 6])

(set (remove nil? (map #{0 1 2 3} #{2 3 4 5})))

(defn reiterate [f x]
  (cons x (lazy-seq (reiterate f (f x)))))
(take 5 (reiterate inc 0))

(defn prod-dig [a b]
  (map #(Character/digit % 10) (str (* 3 7))))
(prod-dig 999 99)

(defn groupby2 [f coll]
  (let [vals (map vec (partition-by f coll))
        keys (map #(f (first %)) vals)

        update-map
        (fn [m [k v]]
          (if (m k)
            (update m k conj (first v))
            (assoc m k v)))]
    (->> (interleave keys vals)
         (partition 2)
         (reduce update-map {}))))
(defn groupby1 [f coll]
  (let [vals (map vec (partition-by f coll))
        keys (map #(f (first %)) vals)

        filter-by-key
        (fn [key]
          (filter #(= (f %) key) coll))]
    (zipmap keys (map filter-by-key keys))))
(defn groupby3 [f coll]
  (apply hash-map
         (map #(vector (f (first %)) %)
              (partition-by f (sort coll)))))
(defn groupby4 [f coll]
  (->> coll
       (map (fn [x] {(f x) [x]}))
       (apply (partial merge-with concat))))

(groupby1 #(> % 5) [1 3 6 8])
(groupby1 #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
(groupby1 count [[1] [1 2] [3] [1 2 3] [2 3]])

(defn sym-diff [a b]
  (let [s (set (filter a b))]
    (set (into (remove s a) (remove s b)))))
(sym-diff #{1 3 5 7} #{1 2 3 4 5 6})

(defn dec->bin [x]
  (loop [n x, res ""]
    (if (= n 0)
      (if (= 0 (count res)) "0" (apply str res))
      (if (even? n)
        (recur (/ n 2) (cons "0" res))
        (recur (/ (- n 1) 2) (cons "1" res))))))
(dec->bin 1365)

(defn expt [x n]
  (reduce * (repeat n x)))
#(reduce * (repeat %2 %1))
(defn bin->dec [bin]
  (reduce +
          (reduce (fn [acc x]
                    (->> acc
                         (count)
                         (expt 2)
                         (* x)
                         (conj acc)))
                  []
                  (->> bin
                       (map str)
                       (reverse)
                       (map #(Integer/parseInt %))))))
(defn bin->dec2 [bin]
  (reduce (fn [x y] (+ (* 2 x) y))
          0
          (map (fn [d] (- (int d) (int \0)))
               (seq bin))))
(bin->dec "10101010101")
(bin->dec "1111111111111111")
(bin->dec "0")
(bin->dec "1")

(defn infix-calc [a & more]
  (reduce (fn [a [f b]] (f a b))
          a (partition 2 more)))
(infix-calc 38 + 48 - 2 / 2)
(infix-calc 10 / 2 - 1 * 2)
(infix-calc 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)

;; Problem 157 - Lots of other interesting solutions at http://www.4clojure.com/problem/solutions/157
(defn idx-seq [coll]
  (reduce
    (fn [acc k] (conj acc [k (count acc)]))
    [] coll))
(idx-seq [0 1 3])
(map-indexed vector [:a :b :c])

(defn factorial [n]
  (reduce * (range 1 (inc n))))
(defn choose [n k]
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))
(defn pascal-tri [n]
  (map #(choose (dec n) %) (range n)))
(factorial 20)
(choose 12 5)
(defn pascal-jb [n]
  (if (= n 1)
    [1]
    (concat [1] (->> n
                     dec
                     pascal-jb
                     (partition 2 1)
                     (map (partial apply +)))
            [1])))
(defn pascal-anjensan [x]
  (nth
    (iterate #(concat [1] (map + % (rest %)) [1]) [1])
    (dec x)))
(pascal-tri 11)
(map pascal-tri (range 1 21))

(defn my-map [f coll]
  (lazy-seq
    (when-not (empty? coll)
      (cons (f (first coll))
            (my-map f (rest coll))))))
(my-map inc [1 2 3 4 5])

(defn is-tree? [coll]
  (cond
    (nil? coll) true
    (not (coll? coll)) false
    :else
    (and (= 3 (count coll))
         (is-tree? (second coll))
         (is-tree? (last coll)))))
(is-tree? '(:a (:b nil nil) nil))
(is-tree? '(:a (:b nil nil)))

(defn square [x] (* x x))

; Problem 120
((fn [c]
   (let [square #(* % %)
         char->dig #(Character/digit % 10)
         digs-sq #(->> % (str) (map char->dig) (map square) (apply +))]
     (count (filter #(< % (digs-sq %)) c))))
 (range 30))

(fn [[s r]]
  {:suit ({\H :heart \S :spade \D :diamond \C :club} s)
   :rank (#_{\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6
             \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12}
          (zipmap "23456789TJQKA" (range)) r)})

(defn sym-tree? [[n l r]]
  (if (and (coll? l) (coll? r))                             ;; Need to do recursive if just one is a coll toO!
    (and (sym-tree? l)                                      ;; Ugh.
         (sym-tree? r)                                      ;; Need a better way to check that branches are equal in any order
         (= (first l) (first r))                            ;; probably just check if their sets are equal
         (or (and (= (second l) (second r))
                  (= (last l) (last r)))
             (and (= (second l) (last r))
                  (= (last l) (second r)))))
    (= l r)))

(defn sym-tree?2 [[_ left right]]
  (letfn [(flip [[n l r]]
            (when-not (nil? n)
              [n (flip r) (flip l)]))]
    (= left (flip right))))

(sym-tree?2 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
             [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])  ;; true

(sym-tree?2 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
             [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])  ;; false

(sym-tree?2 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
             [2 [3 nil [4 [6 nil nil] nil]] nil]])          ;; false


(defn pascal-trap [coll]
  (iterate
    (fn [c]
      (mapv +' (cons 0 c)
            (conj c 0)))
    coll))

(take 5 (pascal-trap [3 1 2]))


(defn tree->table [m]
  (into {} (for [[k1 v1] m
                 [k2 v2] v1]
             {[k1 k2] v2})))

(for [kl1 (keys '{[1] {a b c d}
                  [2] {q r s t u v w x}})]
  (println (get '{[1] {a b c d}
                  [2] {q r s t u v w x}} kl1)))

(tree->table '{a {p 1, q 2}
               b {m 3, n 4}})

(tree->table '{[1] {a b c d}
               [2] {q r s t u v w x}})

(tree->table '{m {1 [a b c] 3 nil}})

;; 153
(defn pw-disj [sos]
  (every? true?
          (for [s1 sos
                s2 (disj sos s1)]
            (and (every? nil? (map s1 s2))
                 (not (and (contains? s1 nil)
                           (contains? s2 nil)))))))
(defn pw-disj2 [sos]
  (= (apply + (map count sos))
     (count (set (mapcat seq sos)))))
(defn pw-disj3 [sos]
  (= (->> sos (map count) (apply +))
     (->> sos (mapcat seq) set count)))

(pw-disj #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
(pw-disj #{#{1 2}
           #{3 4}
           #{5 6}
           #{3}})
(pw-disj #{#{(#(-> *)) + (quote mapcat) #_nil}
           #{'+ '* mapcat (comment mapcat)}
           #{(do) set contains? nil?}
           #{,,, #_,, empty?}})                             ;; false

(defn gcd [a b]
  (if (= 0 b) a (recur b (mod a b))))

(defn lcm [& ns]
  (letfn [(gcd [a b] (if (= 0 b) a (recur b (mod a b))))
          (lcm [a b] (/ (* a b) (gcd a b)))]
    (reduce lcm ns)))

(gcd 48 18)
(lcm 2 3)
(lcm 3/4 1/6)
(lcm 7 3/5)
(lcm 5 3 7)
(lcm 7 5/7 2 3/5)


(defn flip [f]
  (fn [& args]
    (apply f (reverse args))))


((fn [c]
   (->> c
        (group-by type)
        vals
        set)) [1 :a 2 "ter" [3 4]])


((fn [c]
   (->> c
        (group-by identity)
        (map (fn [[k v]] {k (count v)}))
        (into {})))
 [1 1 2 3 2 1 1])

((fn [c]
   (->> c
        (map #(hash-map % 1))
        (apply merge-with +)))
 [1 1 2 3 2 1 1])

;; 56
(fn [c]
  (reduce (fn [acc x]
            (if (some #(= x %) acc)
              acc
              (conj acc x)))
          [] c))

;; 58
(defn mein-comp [& fs]
  (fn [& x]
    (loop [to-run (butlast fs)
           ret (apply (last fs) x)]
      (if (empty? to-run)
        ret
        (recur (butlast to-run) ((last to-run) ret))))))
((mein-comp zero? #(mod % 8) +) 3 5 7 9)

(fn leetwinski-comp [& fs]                                  ;; Wow
  (fn [& a] (let [[f & r] (reverse fs)]
              (reduce #(%2 %1) (apply f a) r))))


(defn my-partition [n c]
  (take-while
    #(= n (count %))
    (reduce (fn [acc x]
              (let [l (last acc)]
                (if (and l (> n (count l)))
                  (conj (vec (butlast acc)) (conj l x))
                  (conj acc [x]))))
            [] c)))
(my-partition 3 (range 8))
(defn partition-anjensan [n c]
  (map #(take n (drop % c))
       (range 0 (- (count c) n -1) n)))


(defn triangles [n]
  (map
    #(println (str (apply str (repeat (- n %) " "))
                   "*"
                   (apply str (repeat % "-*"))))
    (range n)))
(triangles 20)


(defn my-juxt [& fns]
  (fn [& args]
    (for [f fns] (apply f args))))
((my-juxt + max min) 2 3 5 1 6 4)


(defn word-sorting [s]
  (sort-by clojure.string/lower-case (re-seq #"\w+" s)))


(defn get-primes [n]
  (loop [nums (drop 2 (range))
         primes [2]]
    (if (<= n (count primes))
      primes
      (recur (filter #(not= 0 (mod % (last primes))) nums)
             (conj primes (second nums))))))
(defn get-primes-john [pnum]
  (take pnum
        (remove (fn [n]
                  (some (fn [x] (= (rem n x) 0)) (range 2 n)))
                (drop 2 (range)))))
(last (get-primes 1000))
(last (get-primes-john 1000))


(defn filter-squares-hack [s]
  (let [squares (set (map #(* % %) (range 20)))]
    (->> s
         (#(clojure.string/split % #","))
         (map #(Integer. %))
         (filter squares)
         (clojure.string/join ","))))

(defn filter-squares [s]
  (->> (clojure.string/split s #",")
       (map #(Integer. %))
       (filter #(= (Math/sqrt %)
                   (Math/floor (Math/sqrt %))))
       (clojure.string/join ",")))


(defn anagram-finder [c]
  (->> c
       (map #(hash-map (sort %) (set [%])))
       (apply merge-with into)
       vals
       (remove #(= 1 (count %)))
       set))
(anagram-finder ["meat" "mat" "team" "mate" "eat"])


(defn tic-tac-toe-winner-1.0 [rows]
  (let [cols (for [i (range 3)]
               (map #(% i) rows))
        cross [(map #((rows %1) %1) (range 3))
               (map #((rows %1) %2) (range 3) (range 2 -1 -1))]
        triples (concat rows cols cross)
        winners (for [triple triples]
                  (cond
                    (every? #(= :x %) triple) :x
                    (every? #(= :o %) triple) :o))]
    (->> winners (remove nil?) first)))

(defn tic-tac-toe-winner-2.0 [rows]
  (let [cols (apply map list rows)
        cross [(map #((rows %1) %1) (range 3))
               (map #((rows %1) %2) (range 3) (range 2 -1 -1))]
        triples (set (concat rows cols cross))]
    (cond
      (triples [:x :x :x]) :x
      (triples [:o :o :o]) :o)))

(tic-tac-toe-winner-2.0 [[:x :e :o]
                         [:x :e :e]
                         [:x :e :o]])
(tic-tac-toe-winner-2.0 [[:x :e :o]
                         [:x :o :e]
                         [:o :e :x]])


(defn my-reductions
  ([op start coll]
   )
  ([op coll]
   (my-reductions op (first coll) (rest coll))))


























;(eval (list 'let ['result (+ 1 2)]
;            (list 'println 'result)
;            'result))
(eval +)
;(eval (let [test 'nil?
;            body '((println "Yo"))]
;        (list 'if test (cons 'do body))))
(conj '(:c 4) :a)

;(eval `(+ 1 ~(+ 3 4)))

(defmacro code-critic
  "Phrases are courtesy Hermes Conrad from Futurama"
  [bad good]
  (list 'do
        (list 'println
              "Great squid of Madrid, this is bad code:"
              (list 'quote bad))
        (list 'println
              "Sweet gorilla of Manila, this is good code:"
              (list 'quote good))))
(defmacro code-critic2 [bad good]
  `(do
     (println "Great squid of Madrid, this is bad code:" `'~bad)
     (println "Sweet gorilla of Manila, this is good code:" '~good)))

;(code-critic (1 + 1) (+ 1 1))
(macroexpand '(code-critic2 (1 + 1) (+ 1 1)))

(defmacro my-print
  [expression]
  (list 'let ['result expression]
        (list 'println 'result)
        'result))

(defmacro my-print2 [expression]
  `(let [result ~expression]
     (println result)
     result))

;(my-print (* 3 5))
(macroexpand-1 '(my-print2 (* 3 5)))
(macroexpand-1 '(my-print (* 3 5)))

(defmacro my-wand
  [coll]
  `(println ~coll))

;(let [c [(+ 1 2) (+ 2 3)]]
;  (my-wand c))
