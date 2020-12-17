(ns clojurepalooza.bg-stats
  (:require [cheshire.core :as json]))

(def all-stats
  (-> "resources/BGStatsExport.json"
      slurp
      (json/parse-string true)
      keys))

