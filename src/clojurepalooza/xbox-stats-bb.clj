#!/usr/bin/env bb
(import 'java.time.format.DateTimeFormatter
        'java.time.LocalDateTime)

(require '[babashka.curl :as http]
         '[babashka.pods :as pods]
         '[clojure.string :as string])

(pods/load-pod 'retrogradeorbit/bootleg "0.1.9")
(require '[pod.retrogradeorbit.bootleg.utils :as utils]
         '[pod.retrogradeorbit.hickory.select :as s])

(def my-url "https://www.trueachievements.com/gamer/conjurer12/gamecollection")

(defn get-all-column-headers [game-collection-htree]
  "Perhaps some day I can pull all of his data..."
  (->> (s/select (s/id "ddlSortBy") game-collection-htree)
       first
       :content
       (filter map?)
       (mapv (comp first :content))))

(defn get-game-table [game-collection-htree]
  (s/select (s/tag :tr) game-collection-htree))

(defn get-if-content [x]
  (if (map? x)
    ((comp first :content) x)
    x))

(defn get-row-content [row]
  (->> row
       :content
       (filter map?)
       (mapv (comp first :content))
       (mapv get-if-content)))

(defn string-time->minutes [s]
  (->> (string/split s #" ")
       (partition 2)
       (map (fn [[n unit]]
              (let [n (parse-long n)]
                (case unit
                  ("min" "mins") n
                  ("hr" "hrs") (* 60 n)))))
       (apply +)))

(defn string-fraction->decimal [s]
  (as-> s $
        (string/replace $ #"," "")
        (string/split $ #" / ")
        (mapv parse-double $)
        (apply / $)))

(defn string-fraction->numerator [s]
  (-> s
      (string/replace #"," "")
      (string/split #" / ")
      first
      parse-long))

(defn create-game-maps [[col-headers & game-rows]]
  (let [header-names (get-row-content col-headers)]
    (->> game-rows
         (mapv get-row-content)
         (mapv (partial zipmap header-names))
         ;; the last item of the above is the total, maybe that's interesting...
         (filter #(= "Owned" (get % "Ownership")))
         (mapv #(clojure.set/rename-keys % {"TA" "TrueAchievements", "GS" "GamerScore", "%age" "Completion %"}))
         (mapv #(dissoc % "Platform" "Ownership" "My rating" "Contests?" "Last win" nil))
         (mapv #(update % "Time played" string-time->minutes))
         (mapv #(update % "Completion %" parse-double))
         (mapv #(assoc % "TrueAchievement %" (string-fraction->decimal (get % "TrueAchievements"))))
         (mapv #(assoc % "TrueAchievement score" (string-fraction->numerator (get % "TrueAchievements"))))
         )))

(defn sort-titles-by [gm category]
  (->> gm
       (sort-by #(get % category))
       (mapv #(vector (get % category)
                      (get % "Title")))
       reverse
       vec))

(defn println-no-op [x & msgs]
  (apply println msgs)
  x)

(defn get-date []
  (.format (LocalDateTime/now)
           (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss")))

(defn main []
  (println "Fetching gameplay data from TrueAchievements:" my-url)
  (let [outfile (format "resources/xbox-data/game-data-%s.edn" (get-date))]
    (-> (http/get my-url)
        :body
        (println-no-op "Parsing data...")
        (utils/convert-to :hickory)
        get-game-table
        create-game-maps
        (println-no-op "Spitting to file:" outfile)
        ((partial spit outfile)))))

(main)
