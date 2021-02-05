(ns clojurepalooza.clara-rules
  (:require [clara.rules :as rules]))

(defn tag-as [val tag]
  (vary-meta val assoc :type tag))
(defn as-temp [i]
  (tag-as {:value i} :fact/temperature))
(rules/defrule xxxxx
               [?substance <- :fact/substance [{:keys [temperature
                                                       substance-name]}]
                (< temperature 32)
                (= substance-name "water")] => (rules/insert! (tag-as {:substance-name (:substance-name ?substance)
                                                                       :state :solid}
                                                                      :fact/substance-state)))
(rules/defrule get-solids
               [?substance <- :fact/substance-state [{:keys [state]}] (= :solid state)]
               => (println (str "This is solid: " (:substance-name ?substance))))
(let [session (rules/mk-session
               [xxxxx
                get-solids])]
  (-> session
      (rules/insert (tag-as {:temperature 20
                             :substance-name "water"}
                            :fact/substance ))
      (rules/insert (tag-as {:temperature 75
                             :substance-name "cement"}
                            :fact/substance))
      rules/fire-rules))