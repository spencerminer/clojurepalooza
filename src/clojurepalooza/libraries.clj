(ns clojurepalooza.libraries
  (:require [clj-http.client :as http]
            [java-time :as t]))

(http/get "http://cosine.org"
          {:as :json})

(t/format "MM/dd" (t/zoned-date-time (t/minus (t/instant) (t/days 10))))

(-> (t/zoned-date-time)
    (t/minus (t/days 10))
    (->> (t/format "uuuu-MM-dd"))
    )
