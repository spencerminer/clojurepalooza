(ns clojurepalooza.imdb
  (:require [clojure.data.csv :as csv]))

;; Data can be downloaded here: https://www.imdb.com/interfaces/

(def doctor-who "tt0436992")
(def clone-wars "tt0458290")
(def parks-n-rec "tt1266020")
(def mandalorian "tt8111088")
(def rebels "tt2930604")
(def arrested-development "tt0367279")
(def brooklyn-99 "tt2467372")
(def schitts-creek "tt3526078")
(def simpsons "tt0096697")
(def west-wing "tt0200276")

(def show-id west-wing)

(def episodes-tsv "resources/title.episode.tsv")
(def ratings-tsv "resources/title.ratings.tsv")

(defn str->int [s] (Integer. ^String s))
(defn str->float [s] (Float. ^String s))

(defn map-tsv-file [f]
  (let [[header & rows] (-> f slurp (csv/read-csv :separator \tab))
        header (map keyword header)]
    (map (partial zipmap header) rows)))

(def episodes
  (->> (map-tsv-file episodes-tsv)
       (filter #(= show-id (:parentTconst %)))
       (remove #(= "\\N" (:seasonNumber %)))
       (map #(hash-map (keyword (:tconst %))
                       (-> %
                           (select-keys [:seasonNumber :episodeNumber])
                           (update :seasonNumber str->int)
                           (update :episodeNumber str->int))))
       (sort-by (comp :episodeNumber second))
       (sort-by (comp :seasonNumber second))
       (apply merge)))

(def ratings
  (->> (map-tsv-file ratings-tsv)
       (map #(hash-map (keyword (:tconst %))
                       (-> %
                           (select-keys [:averageRating :numVotes])
                           (update :averageRating str->float)
                           (update :numVotes str->int))))
       (apply merge)))

(def rated-episodes
  (vec (clojure.set/intersection (set (keys episodes))
                                 (set (keys ratings)))))

(def episode-ratings
  (merge-with merge
              (select-keys episodes rated-episodes)
              (select-keys ratings rated-episodes)))

(def top-10
  (->> episode-ratings
       (sort-by (comp - :averageRating second))
       (take 10)))

(defn print-google-sheets-import-format []
  (->> episode-ratings
       vals
       (sort-by :episodeNumber)
       (sort-by :seasonNumber)
       (map #(vector (format "s%02de%02d"
                             (:seasonNumber %) (:episodeNumber %))
                     (:averageRating %)))
       (map (partial apply println))
       doall
       ((constantly true))))

(comment
 (print-google-sheets-import-format)
 top-10
 )




(comment
 ;; Unused Bash for splitting huge csv
 ;; split -l 500000 title.basics-smaller.tsv
 ;; move the split files into a new folder
 ;; for filename in $(ls);
 ;;   do mv $filename
 ;;         $(echo title.basics-split-$filename.tsv);
 ;; done;
 )
