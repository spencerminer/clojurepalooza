(ns clojurepalooza.transducers)

(def tx-identity
  (fn [rf]
    (fn
      ;; 0-ary: identity
      ([] (rf))
      ;; 1-ary: final
      ([result]
       (rf result))
      ;; 2-ary: process element
      ([result item]
       (rf result item)))))

(defn slow-into [to xf from]
  (transduce xf conj to from))

(defn debug
  ([indent]
   (fn [rf]
     (fn
       ;; 0-ary: identity
       ([] (rf))
       ;; 1-ary: final
       ([result]
        (rf result))
       ;; 2-ary: process element
       ([result item]
        (let [out (rf result item)]
          (println (apply str (repeat indent " ")) out)
          out)))))
  ([in-str out-str]
   (fn [rf]
     (fn
       ;; 0-ary: identity
       ([] (rf))
       ;; 1-ary: final
       ([result]
        (rf result))
       ;; 2-ary: process element
       ([result item]
        (println in-str item)
        (let [out (rf result item)]
          (println out-str out)
          out))))))

(defn progress!
  "Call a side-effecting handler every interval items."
  [interval handler]
  (fn [rf]
    (let [n (volatile! 0)]
      (fn ([]
           (do (vreset! n 0)
               (rf)))
        ([r]
         (handler @n)
         (rf r))
        ([r v]
         (do (when (zero? (mod (vswap! n inc) interval))
               (handler @n))
             (rf r v)))))))

(comment
  (transduce (comp (map inc)
                   (progress! 5 (partial println "Mapped"))
                   (filter even?)
                   (progress! 3 (partial println "Filtered")))
             conj []
             (range 17))

  (slow-into [] (debug "in" "out") (range 3))
  (slow-into [] (debug 2) (range 3))

  (transduce (comp tx-identity
                   (partition-all 5)) conj [] (range 7))
  )