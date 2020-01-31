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
                      {miner 50}))]
  miner-totals)

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