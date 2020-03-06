(ns clojurepalooza.blockchain
  (:import (clojure.lang PersistentQueue)
           (java.util UUID)))

(def the-chain
  [{:previous-hash "GENESIS"
    :data []
    :height 0
    :miner "Spencer"}
   {:previous-hash
    (hash {:previous-hash "GENESIS"
           :data []
           :height 0
           :miner "Spencer"})
    :data [{:from "Spencer" :to "JohnJ" :amount 10 :fee 0.000001}
           {:from "Spencer" :to "Nacho" :amount 10 :fee 0.000001}
           {:from "Spencer" :to "Matt" :amount 10 :fee 0.000001}]
    :height 1
    :miner "Nacho"}])

(def mining-difficulty 0xFFFFF000)

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

(defn is-valid-salt? [block difficulty salt]
  (->> salt
       (assoc block :salt)
       hash
       (bit-and difficulty)
       zero?))
(defn mine-single-thread [n block]
  (->> (repeatedly n #(UUID/randomUUID))
       (filter (partial is-valid-salt? block mining-difficulty))
       first))
(defn mine-parallel [block]
  (->> (range)
       (partition 1000)
       (pmap (fn [chunk]
               (->> chunk
                    (map (juxt identity #(hash (assoc block :salt %))))
                    (filter #(zero? (bit-and mining-difficulty (second %))))
                    first)))
       (filter some?)
       first))
(comment
  (do
    (println "single-threaded: ")
    (def the-solution (time (mine-single-thread 100000 (last the-chain))))
    ;; 7.5 seconds on my mac
    (println the-solution)
    (format "%08x" (hash (assoc (last the-chain) :salt the-solution))))
  (do
    (println "parallel: ")
    (def the-solution (time (mine-parallel (last the-chain))))
    ;; 2 seconds on my mac
    (println the-solution)
    (format "%08x" (hash (assoc (last the-chain) :salt the-solution))))
  ;; check your answer:
  (format "%08x" (hash (assoc (last the-chain) :salt the-solution)))
  )

(def print-lock (Object.))
(declare broadcast)
(defn thp [& args]
  (locking print-lock
    (apply println args)))

(defn run-miner [stop i inputstate]
  (let [[[msg] _] (swap-vals! inputstate pop)]
    (cond
      (and (uuid? msg)
           (is-valid-salt? (last the-chain)
                           mining-difficulty
                           msg))
      (thp i ": Wow, you beat me. Thou art the better miner")   ;;(start-next-block)

      (= msg :stop)
      (thp "Stopping" i "!!!")

      :else
      (do
        (when msg
          (thp (format "I'm mining (%d) and the message is %s!"
                       i msg)))
        (when-let [result (mine-single-thread 1000 (last the-chain))]
          (thp i ":" result)
          (broadcast result))
        (recur stop i inputstate)))))

(defn miner [i]
  (let [stop (atom false)
        inputstate (atom (PersistentQueue/EMPTY))]
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

  ;; Evaluate this to run all the miners
  miners

  (send-message-to-miner :wassup! (nth miners 5))
  (send-message-to-miner :stop (nth miners 3))
  (broadcast :hellooooo-everyone)
  (broadcast :stop)

  )