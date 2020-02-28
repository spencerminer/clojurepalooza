(ns clojurepalooza.blockchain)

(def the-chain
  [{:previous-hash "GENESIS"
    :data []
    :height 0
    :miner "Spencer"}
   {:previous-hash (hash {:previous-hash "GENESIS"
                          :data []
                          :height 0
                          :miner "Spencer"})
    :data [{:from "Spencer" :to "JohnJ" :amount 10 :fee 0.000001}
           {:from "Spencer" :to "Nacho" :amount 10 :fee 0.000001}
           {:from "Spencer" :to "Matt" :amount 10 :fee 0.000001}]
    :height 1
    :miner "Nacho"}])

(let [miner-totals (apply merge-with +
                          (for [{:keys [miner]} the-chain]
                            {miner 50}))

      transactions (mapcat (fn [{:keys [data miner]}]
                             (mapcat (fn [{:keys [from to amount fee]}]
                                       [{from (- amount)}
                                        {to amount}
                                        {from (- fee)}
                                        {miner fee}])
                                     data)) the-chain)]
  (apply merge-with + (concat [miner-totals] transactions)))

;; a series of tubes
#_(->> (for [salt (range)]
         [salt (hash (assoc block :salt salt))])
       (filter #(= 0 (bit-and 0xFFFFFF80 (second %))))
       ffirst)
(defn mine-single-thread [n block]
  (->> (repeatedly n #(java.util.UUID/randomUUID))
       (map (juxt identity #(hash (assoc block :salt %))))
       (filter #(zero? (bit-and 0xFFFFF000 (second %))))
       first))
(defn mine-parallel [block]
  (->> (range)
       (partition 1000)
       (pmap (fn [chunk]
               (->> chunk
                    (map (juxt identity #(hash (assoc block :salt %))))
                    (filter #(zero? (bit-and 0xFFFFFC00 (second %))))
                    first)))
       (filter some?)
       first))
(comment
  (do
    (println "single-threaded: ")
    (def the-solution (time (mine-single-thread 100000 (last the-chain))))
    ;; 7.5 seconds on my mac
    (println))
  (do
    (println "parallel: ")
    (def the-solution (time (mine-parallel (last the-chain))))
    ;; 2 seconds on my mac
    (println))
  ;; check your answer:
  (format "%08x" (hash (assoc (last the-chain) :salt (first the-solution))))
  )

(let [miner-totals (apply merge-with +
                          (for [{:keys [miner]} the-chain]
                            {miner 50}))

      transactions (mapcat (fn [{:keys [data miner]}]
                             (mapcat (fn [{:keys [from to amount fee]}]
                                       [{from (- amount)}
                                        {to amount}
                                        {from (- fee)}
                                        {miner fee}])
                                     data)) the-chain)]
  (apply merge-with + (concat [miner-totals] transactions)))

(def print-lock (Object.))
(defn thp [& args]
  (locking print-lock
    (apply println args)))
(defn run-miner [stop i inputstate]
  (let [[[msg] _] (swap-vals! inputstate pop)]
    (if (= msg :stop)
      (thp "Stopping" i "!!!")
      (do
        (when msg
          (thp (format "I'm mining (%d) and the message is %s!"
                       i msg)))
        (when-let [result (mine-single-thread 1000 (last the-chain))]
          (thp i ": " result)
          (broadcast :stop))
        (recur stop i inputstate)))))

(defn miner [i]
  (let [stop (atom false)
        inputstate (atom (clojure.lang.PersistentQueue/EMPTY))]
    {:thread (future
               (thp (format "Starting miner %d!" i))
               (run-miner stop i inputstate))
     :inputstate inputstate}))

(def miners (for [i (range 10)]
              (miner i)))
(defn send-message-to-miner [x m]
  (swap! (:inputstate m) conj x))
(defn broadcast [x]
  (doseq [m miners]
    (send-message-to-miner x m)))

(comment

  miners
  (send-message-to-miner :wassup! (nth miners 5))
  (send-message-to-miner :stop (nth miners 3))
  (broadcast :hellooooo-everyone)
  (broadcast :stop)

  )