(ns clojurepalooza.lock-free-concurrency)

(let [a (agent {})]
  (send a assoc :k "v")
  (deref a))
;; => {}

(let [a (agent {})]
  (send a assoc :k "v")
  (Thread/sleep 1)
  (deref a))
;; => {:k "v"}

(doseq [_ (range 1000)]
  (future
   (prn "a"
        {:foo "bar"}
        [10 9 8 7 6 5 4 3 2 1]
        "Put some more test here.....")))
;; JuMbLEd

(let [printer   (future (prn "five"))
      printer-2 (future (prn "five-two"))
      printer-3 (future (prn "five-two-three"))]
  @printer
  @printer-2
  @printer-3)
;; also jumbled, somewhat surprisingly


(let [lockfree-prn-agent (agent nil)]
  (defn lockfree-prn
    [& x]
    (let [current-out *out*]
      (send lockfree-prn-agent
            (fn [_]
              (binding [*out* current-out]
                (apply prn x))))
      nil))

  (doseq [_ (range 1000)]
    (future
     (lockfree-prn "a"
                   {:foo "bar"}
                   [10 9 8 7 6 5 4 3 2 1]
                   "Put some more test here....."))))

(let [lockfree-prn-agent (agent nil)]
  (defn lockfree-prn
    [& x]
    (send lockfree-prn-agent
          (fn [_] (apply prn x)))
    nil)

  (doseq [_ (range 1000)]
    (future
     (lockfree-prn "a"
                   {:foo "bar"}
                   [10 9 8 7 6 5 4 3 2 1]
                   "Put some more test here....."))))
