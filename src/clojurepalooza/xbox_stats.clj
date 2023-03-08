(ns clojurepalooza.xbox-stats
  (:require [clj-http.client :as http]
            [clj-time.core :as t]
            [clojure.string :as string]
            [hickory.core :as h]
            [hickory.select :as s]))

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

(defn println-no-op [x msg]
  (println msg)
  x)

(defn main []
  (println "Fetching gameplay data from TrueAchievements:" my-url)
  (-> (http/get my-url)
      :body
      (println-no-op "Parsing data...")
      h/parse
      h/as-hickory
      get-game-table
      create-game-maps
      (println-no-op "Spitting to file")
      ((partial spit (format "resources/xbox-data/game-data-%s.edn" (t/now))))))

#_(main)

(comment
 (def gm (create-game-maps gch))
 (-> gm
     first
     keys)

 ;; LinuxStation
 ;; - good for having a full desktop anywhere in the world
 ;; - Needs dual-core intel CPU, 2 gig DDR3 memory
 ;; Virtualization Station
 ;; - Needs dual-core (ideally quad) intel CPU, 2 gig (ideally 4) DDR3 memory
 ;; - can do anything basically
 ;; Container Station
 ;; - small, fewer resources required
 ;; -

 ;;=>
 ;;("TrueAchievement"
 ;; "Play Status"
 ;; "Title"
 ;; "Achievements"
 ;; "Date started"
 ;; "Time played"
 ;; "Completion %age"
 ;; "Gamerscore"
 ;; "Date completed")
 (sort-titles-by gm "Time played")
 (sort-titles-by gm "Completion %age")
 (sort-titles-by gm "TrueAchievement")
 (sort-titles-by gm "TrueAchievement %age")
 (sort-titles-by gm "TrueAchievement score"))
