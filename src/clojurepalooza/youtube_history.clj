(ns clojurepalooza.youtube-history
  (:require [hickory.core :as h]
            [hickory.select :as s]))

(def parsed-history-work
  (-> "resources/watch-history.html" slurp h/parse))

(def parsed-history-personal
  (-> "resources/watch-history-personal.html" slurp h/parse))

(defn get-history-elements [parsed-history]
  (-> parsed-history
      h/as-hickory
      :content                                              ;; document
      first                                                 ;; html
      :content
      second                                                ;; body
      :content
      first                                                 ;; div
      :content))

(defn get-title-from-entry [entry]
  (-> entry
      :content
      first
      :content
      second
      :content
      second
      :content
      first))

(defn get-channel-from-entry [entry]
  (some-> entry
          :content
          first
          :content
          second
          :content
          ((fn [stuff]
             (if (<= 4 (count stuff))
               (nth stuff 3)
               (clojure.pprint/pprint stuff))))
          :content
          first))

(comment

 (->> parsed-history-personal
      get-history-elements
      (map get-title-from-entry)
      count)

 (->> parsed-history-personal
      get-history-elements
      (map get-channel-from-entry)
      frequencies
      (sort-by second))

 (->> parsed-history-work
      get-history-elements
      (map get-title-from-entry))

 (->> parsed-history-work
      get-history-elements
      (map get-channel-from-entry)
      frequencies
      (sort-by second)))
