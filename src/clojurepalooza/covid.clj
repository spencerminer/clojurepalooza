(ns clojurepalooza.covid
  (:require [clj-http.client :as http]
            [clj-time.format :as f]
            [clj-time.core :as t]
            [clojure.data.csv :as csv]
            [clojure.string :as str]))

(defn daily-report-filename [day]
  (format (str "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/"
               "master/csse_covid_19_data/csse_covid_19_daily_reports/%s.csv")
          day))

(defn nice-keyword [h]
  (-> h
      str/trim
      (str/replace #"^\W" "")
      (str/replace #"\W" "-")
      str/lower-case
      keyword))

(defn numeric [v]
  (if (str/blank? v)
    0
    (Integer/parseInt v)))

(defn numerics [r]
  (-> r
      (update :confirmed numeric)
      (update :deaths numeric)
      (update :recovered numeric)))

(defn tseq [t]
  (lazy-seq (cons t (tseq (t/plus t (t/days 1))))))

(def filename-parser (f/formatter "MM-dd-YYYY"))

(defn format-filename-date [t] (f/unparse filename-parser t))

(defn -read-csv [day]
  (let [[header & rows]
        (-> day
            format-filename-date
            daily-report-filename
            http/get
            :body
            csv/read-csv)
        header (map nice-keyword header)]
    (->> rows
         (map #(zipmap header %))
         (map (partial merge {:day day}))
         (map numerics))))

(def read-csv (memoize -read-csv))

(defn read-csvs [max-days]
  (let [days (tseq (t/date-time 2020 1 22))]
    (group-by (juxt :country-region :province-state)
              (mapcat read-csv (take max-days days)))))

(sort-by
  :surviving
  (vals (reduce-kv (fn [m k v]
                     (let [{:keys [deaths confirmed] :as full-data}
                           (->> v
                                (sort-by :day)
                                last)]
                       (assoc m k
                                (assoc full-data
                                  :surviving (- confirmed deaths)))))
                   {}
                   (read-csvs 75))))