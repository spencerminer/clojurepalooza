(ns clojurepalooza.youtube-history
  (:require [hickory.core :as h]
            [hickory.select :as s]))

(def parsed-history
  (-> "resources/watch-history.html" slurp h/parse))

(def history-elements
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
  (-> entry
      :content
      first
      :content
      second
      :content
      (nth 3)
      :content
      first))

(map get-title-from-entry history-elements)
(->> history-elements
     (map get-channel-from-entry)
     frequencies
     (sort-by second))
